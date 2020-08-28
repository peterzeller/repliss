package crdtver.symbolic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import crdtver.RunArgs
import crdtver.language.InputAst.BuiltInFunc.BF_and
import crdtver.language.InputAst.{Forall, NoSource}
import crdtver.language.TypedAst._
import crdtver.language.{InvariantTransform, TypedAst}
import crdtver.symbolic.ExprTranslation.translateType
import crdtver.symbolic.IsabelleTranslation.createIsabelleDefs
import crdtver.symbolic.ModelExtraction.{extractModel, printModel}
import crdtver.symbolic.PredicateAbstraction.{assumeWellformed, makeUniqueIdConstraints, monotonicGrowth}
import crdtver.symbolic.SVal._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.SymbolicEvaluator.{makeGeneratedIdsVar, makeKnownIdsVar}
import crdtver.symbolic.SymbolicMapVar.symbolicMapVar
import crdtver.symbolic.SymbolicSort._
import crdtver.testing.Interpreter
import crdtver.testing.Visualization.RenderResult
import crdtver.utils.ConcurrencyUtils
import crdtver.utils.PrettyPrintDoc.Doc
import crdtver.utils.StringUtils._

import scala.collection.mutable
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.util.{Failure, Success}
import scala.xml.Elem

/** translation of the checked formulas */
case class Translation(
  name: String,
  isabelleTranslation: String,
  cvcTranslation: String,
  smtTranslation: String
) {
  val toXml: Elem = <translation name={name}>
    <isabelle>
      {isabelleTranslation}
    </isabelle>
    <smt>
      {smtTranslation}
    </smt>
    <cvc>
      {cvcTranslation}
    </cvc>
  </translation>

}

case class SymbolicExecutionRes(
  proc: String,
  time: Duration,
  error: Option[SymbolicCounterExample],
  exception: Option[Throwable],
  translations: List[Translation]
)

class SymbolicExecutionException(
  val counterExample: SymbolicCounterExample
) extends RuntimeException(counterExample.message) {
  override def toString: String = s"SymbolicExecutionError:\n$counterExample"
}

class SymbolicEvaluator(
  val originalProg: InProgram,
  runArgs: RunArgs
) {

  val prog: InProgram = InvariantTransform.transformProg(originalProg)

  val modelPath: Path = initModelPath()


  def checkInitialState(): SymbolicExecutionRes = {
    val ctxt = newCtxt("InitialState")
    val state = SymbolicState(
      calls = SymbolicMapEmpty(implicitly, SCallInfoNone()),
      happensBefore = SymbolicMapEmpty(implicitly, SSetEmpty(implicitly)),
      callOrigin = SymbolicMapEmpty(implicitly, SNone(implicitly)),
      transactionOrigin = SymbolicMapEmpty(implicitly, SNone(implicitly)),
      generatedIds = Map(),
      knownIds = Map(),
      invocationCalls = SymbolicMapEmpty(implicitly, SSetEmpty(implicitly)),
      invocationOp = SymbolicMapEmpty(implicitly, SInvocationInfoNone()),
      invocationRes = SymbolicMapEmpty(implicitly, SReturnValNone()),
      currentInvocation = ctxt.makeVariable("currentInvoc"),
      currentTransaction = None,
      localState = Map(),
      visibleCalls = SSetEmpty(implicitly),
      currentCallIds = List(),
      satisfiable = true,
      trace = Trace(),
      internalPathConditions = List(),
      snapshotAddition = SSetEmpty(implicitly),
      translations = List(),
    )
    val res: CheckInvariantResult = checkInvariant(NoSource(), ctxt, state, "in initial state")
    val example = res.firstCounterExample
    SymbolicExecutionRes(
      "in initial state",
      Duration.Zero,
      example,
      None,
      state.translations
    )
  }

  def checkProgram(): LazyList[SymbolicExecutionRes] = {
    debugPrint("checking program")
    val timeoutPerProc: Duration = runArgs.timeout
    checkInitialState() #::
      (for (proc <- prog.procedures.to(LazyList)) yield checkProcedure(proc, timeoutPerProc))
  }


  private def initModelPath(): Path = {
    val modelPath = Paths.get(".", "model", prog.name)
    modelPath.toFile.mkdirs()
    for (f <- modelPath.toFile.listFiles())
      f.delete()
    modelPath
  }

  private def writeOutputFile(fileName: String, fileContents: String): Unit = {
    var path: Path = modelPath.resolve(fileName)
    var i: Int = 0
    while (path.toFile.exists()) {
      i += 1
      path = modelPath.resolve(fileName.insertBeforeDot(i.toString))
    }
    Files.write(path, fileContents.getBytes(StandardCharsets.UTF_8))
  }

  private def checkProcedure(proc: TypedAst.InProcedure, timeout: Duration): SymbolicExecutionRes = {
    ConcurrencyUtils.withTimeout[SymbolicExecutionRes, SymbolicExecutionRes](
      timeout = timeout,
      name = s"check-procedure ${proc.name}",
      work = checkProcedure2(proc),
      onDone = {
        case (Success(r), d) => r.copy(time = d)
        case (Failure(e), d) =>
          val message = e match {
            case t: TimeoutException =>
              s"Timed out after ${d}"
            case _ =>
              e.getMessage
          }
          SymbolicExecutionRes(
            proc.name.name,
            d,
            Some(SymbolicCounterExample(
              message,
              proc.source.range,
              Trace(List()),
              None,
              Translation("", "", "", "")
            )),
            Some(e),
            List())
      })
  }

  private def checkProcedure2(proc: TypedAst.InProcedure): SymbolicExecutionRes = {
    try {
      debugPrint(s"checking procedure ${proc.name}")
      debugPrint(s"proc:\n${proc.printAst}")
      implicit val ctxt: SymbolicContext = newCtxt(proc.name.name)

      val params = makeVariablesForParameters(ctxt, proc.params)

      val generatedIds: Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] =
        makeGeneratedIdsVar(ctxt)
      val knownIds: Map[IdType, SVal[SortSet[SortCustomUninterpreted]]] =
        makeKnownIdsVar(ctxt)


      // in the beginning everything is unknown so we use symbolic variables:
      val state: SymbolicState = SymbolicState(
        calls = symbolicMapVar("calls"),
        //[SortCallId, SortOption[SortCall], Nothing](ctxt.makeVariable("calls"))(default()),
        happensBefore = symbolicMapVar("happensBefore"),
        callOrigin = symbolicMapVar("callOrigin"),
        transactionOrigin = symbolicMapVar("transactionOrigin"),
        //        transactionStatus = symbolicMapVar("transactionStatus"),
        generatedIds = generatedIds,
        knownIds = knownIds,
        invocationCalls = symbolicMapVar("invocationCalls"),
        invocationOp = symbolicMapVar("invocationOp"),
        invocationRes = symbolicMapVar("invocationRes"),
        currentInvocation = ctxt.makeVariable("currentInvocation"),
        localState = params.toMap,
        visibleCalls = SSetEmpty(implicitly),
        trace = Trace(),
        snapshotAddition = SSetVar(ctxt.makeVariable("snapshotAddition")),
        translations = List()
      )

      var constraints = mutable.ListBuffer[NamedConstraint]()

      constraints ++= makeUniqueIdConstraints

      // there are a few restrictions we can assume for the initial state:
      // this follows the begin-invoc rule

      // >> invocationOp S i = None;
      constraints += NamedConstraint("i_fresh", 1,
        SMapGet(state.invocationOp, state.currentInvocation) === SInvocationInfoNone())

      // >> procedure (prog S) procName args ≜ (initState, impl);
      // >>   uniqueIdsInList args ⊆ knownIds S';
      // >>   state_wellFormed S';
      // >>   ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
      val var_tx = ctxt.makeBoundVariable[SortTxId]("tx")

      // >>   invariant_all S';
      constraints ++= invariant("before_procedure_invocation", state)(ctxt)
      // >>   invocationOp S' i = None;
      val i = state.currentInvocation

      // >>   prog S' = prog S;
      // >>   S'' = (S'⦇localState := (localState S')(i ↦ initState),
      // this is handled by makeVariablesForParameters
      // >>   currentProc := (currentProc S')(i ↦ impl),
      // see proc
      // >>   visibleCalls := (visibleCalls S')(i ↦ {}),
      // see state.visibleCalls
      // >>   invocationOp := (invocationOp S')(i ↦ (procName, args)) ⦈);
      // >>   ⋀tx. transactionOrigin S'' tx ≠ Some i
      constraints += NamedConstraint("no_transaction_in_new_invocation", 1, {
        val tx = ctxt.makeVariable[SortTxId]("tx", true)
        forall(tx, state.transactionOrigin.get(tx) !== SSome(state.currentInvocation))
      })


      // there are no calls in the current invocation:
      constraints += NamedConstraint("no_call_in_new_invocation", 1,
        state.invocationCalls.get(i) === SSetEmpty(implicitly))

      constraints ++= assumeWellformed("before_procedure_invocation", state, ctxt)

      val args: List[SVal[SortValue]] = params.map(_._2)
      // >>   valid = invariant_all S'';  ― ‹  TODO check invariant in C ?  ›
      val invocationInfo: SVal[SortInvocationInfo] = SInvocationInfo(proc.name.name, args)

      val state2 = state.copy(
        invocationOp = state.invocationOp.put(i, invocationInfo),
        //SymbolicMapUpdated(i, invocationInfo, state.invocationOp)
      ).withConstraints(constraints.toList)
        .withTrace(s"Invocation of ${proc.name}(${args.mkString(", ")})", proc.getSource)

      // check the invariant in state2:
      val ir = checkInvariant(proc.getSource, ctxt, state2, s"directly after invocation of ${proc.name}")
      ir.ifCounterExample(ce => throw new SymbolicExecutionException(ce))


      val state3 = state2.withInvariantResult(ir)
      //      match {
      //        case Some(msg) =>
      //          throw new SymbolicExecutionException(msg)
      //        case None =>
      //        // ok
      //      }

      // continue evaluating the procedure body:
      val finalState: SymbolicState = executeStatement(proc.body, state3, ctxt, s => s)
      SymbolicExecutionRes(
        proc.name.name,
        Duration.Zero,
        None,
        None,
        finalState.translations
      )
    } catch {
      case e: SymbolicExecutionException =>
        SymbolicExecutionRes(
          proc.name.name,
          Duration.Zero,
          Some(e.counterExample),
          None,
          List(e.counterExample.translation)
        )
    }


  }


  private def newCtxt(name: String): SymbolicContext = {
    val smtTranslation = new ToSmtTranslation()
    implicit val ctxt: SymbolicContext = new SymbolicContext(smtTranslation, name, prog, runArgs)
    smtTranslation.datatypeImpl = ctxt.datypeImpl
    ctxt
  }

  def executeStatements(stmts: List[InStatement], state: SymbolicState, ctxt: SymbolicContext, follow: SymbolicState => SymbolicState): SymbolicState = stmts match {
    case Nil =>
      follow(state)
    case x :: xs =>
      executeStatement(x, state, ctxt, executeStatements(xs, _, ctxt, follow))
  }


  def debugPrint(str: => String): Unit = {
    //    println(str)
  }

  def newIdConstraints(state: SymbolicState, vname: String, idType: IdType, newV: SVal[SortCustomUninterpreted]): Iterable[NamedConstraint] = {
    val result = mutable.ListBuffer[NamedConstraint]()
    result += NamedConstraint(s"${vname}_new_id_fresh", 1,
      state.generatedIds(idType)(newV).isNone)

    result
  }

  private def executeStatement(stmt: InStatement, state: SymbolicState, ctxt: SymbolicContext, follow: SymbolicState => SymbolicState): SymbolicState = {
    if (Thread.currentThread().isInterrupted)
      throw new InterruptedException

    implicit val istate: SymbolicState = state
    stmt match {
      case TypedAst.BlockStmt(source, stmts) =>
        debugPrint(s"Executing block in line ${source.getLine}")
        executeStatements(stmts, state, ctxt, follow)
      case TypedAst.Atomic(source, body) =>
        debugPrint(s"Executing begin-atomic in line ${source.getLine}")
        val state2 = executeBeginAtomic(source, state, ctxt)
        executeStatement(body, state2, ctxt, (state3) => {
          debugPrint(s"Executing end-atomic in line ${source.range.stop.line}")
          val state4 = executeEndAtomic(state3, ctxt)

          // assume state 4 wellformed
          assumeWellformed("after_transaction", state4, ctxt)
          // check invariant in state4
          val ir = checkInvariant(source, ctxt, state4, s"When committing transaction of atomic block in line ${source.getLine}.")
          ir.ifCounterExample(c => throw new SymbolicExecutionException(c))

          follow(state4.withInvariantResult(ir))
        })
      case TypedAst.LocalVar(source, variable) =>
        debugPrint(s"Executing local variable $variable in line ${source.getLine}")
        // nothing to do
        state.withTrace(s"Local variable $variable", variable.getSource)
      case TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
        debugPrint(s"Executing if-statement in line ${source.getLine}")
        val condV: SVal[SortBoolean] = ExprTranslation.translate(cond)(bool, ctxt, state)
        // first assume the condition is true
        val ifTrueState = state.withConstraint("if_statement_condition_true", condV)
        ctxt.check(ifTrueState.pathConditions, s"if-statement-true-line-${source.getLine}", false) match {
          case Unsatisfiable(_) =>
          // then-branch cannot be taken
          case Unknown | _: Satisfiable =>
            debugPrint(s"Executing then-statement in line ${thenStmt.getSource.getLine}")
            val state2 = ifTrueState.withTrace(s"Executing then-statement", thenStmt)
            executeStatement(thenStmt, state2, ctxt, follow)
        }

        // next assume the condition is false:
        debugPrint(s"Executing else-statement in line ${elseStmt.getSource.getLine}")
        val ifFalseState = state.withConstraint("if_statement_condition_false", SNot(condV))
        ctxt.check(ifFalseState.pathConditions, s"if-statement-false-line-${source.getLine}", false) match {
          case Unsatisfiable(_) =>
            // else-branch cannot be taken
            ifFalseState.copy(satisfiable = false)
          case Unknown | _: Satisfiable =>
            val state2 = ifFalseState.withTrace("Executing else-statement", elseStmt)
            executeStatement(elseStmt, state2, ctxt, follow)
        }
      case TypedAst.MatchStmt(source, expr, cases) =>
        debugPrint(s"Executing match-statement in line ${stmt.getSource.getLine}")
        val exprSort = translateType(expr.getTyp)(ctxt)
        // first evaluate the expression
        val exprRes = ExprTranslation.translate(expr)(exprSort, ctxt, state)
        var previousCasesFalse: List[NamedConstraint] = List()

        val caseResults = for ((c, ci) <- cases.zipWithIndex) yield {
          // create symbolic variables for pattern
          val vars: List[VarUse] = c.pattern.freeVars().toList
          var state2 = state
          val sVars = for (v <- vars) yield {
            val sv = ctxt.makeVariable(v.name)(translateType(v.typ)(ctxt))
            state2 = state2.withLocal(ProgramVariable(v.name), sv)
            sv
          }

          val patternExpr = ExprTranslation.translate(c.pattern)(exprSort, ctxt, state2)

          // add path condition: previous cases false
          state2 = state2.withConstraints(previousCasesFalse)

          // assume pattern satisfied
          state2 = state2.withConstraint("Case satisfied", exprRes === patternExpr)

          // execute case body
          val execRes = executeStatement(c.statement, state2, ctxt, follow)

          def makeCaseNegCond: SVal[SortBoolean] = {
            var state2 = state
            val sVars = for (v <- vars) yield {
              val sv = ctxt.makeBoundVariable(v.name)(translateType(v.typ)(ctxt))
              state2 = state2.withLocal(ProgramVariable(v.name), sv)
              sv
            }
            val patternExpr = ExprTranslation.translate(c.pattern)(exprSort, ctxt, state2)
            !existsL(sVars, exprRes === patternExpr)
          }

          previousCasesFalse ::= NamedConstraint(s"Case_${ci}_not_taken", 1, makeCaseNegCond)
          execRes
        }

        // TODO completeness check
        // seems like CVC4 needs some help here -- input is one of the datatype cases
        //        println(s"Checking pattern completeness: \n - ${previousCasesFalse.mkString("\n - ")}")
        ctxt.check(previousCasesFalse, "Patterns complete", false) match {
          case Unsatisfiable(unsatCore) =>
          // all cases covered
          case SymbolicContext.Unknown =>
            throwSymbolicCounterExample("Could not prove that all cases in case statement are covered.", source.range, state.withConstraints(previousCasesFalse), None, ctxt)
          case Satisfiable(incomplete, model) =>
            val msg =
              if (incomplete)
                "Could not prove that all cases in case statement are covered (produced counter example does not consider all constraints)."
              else
                s"Not all cases in case statement are covered."
            throwSymbolicCounterExample(msg, source.range, state.withConstraints(previousCasesFalse), Some(model), ctxt)
        }
        caseResults.head
      case TypedAst.CrdtCall(source, call) =>
        debugPrint(s"Executing CRDT call in line ${source.getLine}")
        val t: SVal[SortTxId] = state.currentTransaction.get
        val c: SymbolicVariable[SortCallId] = ctxt.makeVariable("c" + state.currentCallIds.size)
        // assume new call c is distinct from all others:
        val newConstraints = List(
          NamedConstraint(s"${c.name}_freshA", 1, SDistinct(c :: state.currentCallIds)),
          NamedConstraint(s"${c.name}_freshB", 1, SEq(state.calls.get(c), SCallInfoNone()))
        )

        // TODO maybe choose type based on query type
        // TODO assume query specification for res

        val callInfo: SVal[SortCall] = ExprTranslation.translateUntyped(call)(ctxt, state).cast[SortCall]


        val newCurrentCallIds = state.currentCallIds :+ c

        val newVis = state.visibleCalls + c
        val state2 = state.copy(
          calls = SNamedVal("calls", state.calls.put(c, callInfo)),
          currentCallIds = newCurrentCallIds,
          callOrigin = (SNamedVal("callOrigin", state.callOrigin.put(c, SSome(t)))),
          visibleCalls = SSetVar(SNamedVal("vis", newVis)),
          happensBefore = (SNamedVal("happensBefore", state.happensBefore.put(c, state.visibleCalls))),
          invocationCalls = state.invocationCalls.put(state.currentInvocation, SVal.makeSet(newCurrentCallIds))
        ).withTrace(s"call $call", source)
          .withConstraints(newConstraints)

        follow(state2)
      case TypedAst.Assignment(source, varname, expr) =>
        debugPrint(s"Executing assignment in line ${source.getLine}: ${stmt.printAst}")
        // use a new variable here to avoid duplication of expressions
        val v = ctxt.makeVariable(varname.name)(ctxt.translateSortVal(expr.getTyp))
        val state2 = state.copy(
          localState = state.localState + (ProgramVariable(varname.name) -> v)
        ).withTrace(s"assignment $varname", source)
          .withConstraint(s"${v.name}_assignment",
            v === ctxt.translateExprV(expr))
        follow(state2)
      case TypedAst.NewIdStmt(source, varname, typename) =>
        debugPrint(s"Executing new-id statement in line ${source.getLine}")
        val idType = typename.asInstanceOf[IdType]
        val vname = varname.name
        val newV: SVal[SortCustomUninterpreted] = ctxt.makeVariable(vname)(ctxt.translateSort(typename)).asInstanceOf[SVal[SortCustomUninterpreted]]
        val state2 = state.copy(
          localState = state.localState + (ProgramVariable(vname) -> newV)
        ).withTrace(s"New-id $varname", source)
          .withConstraints(newIdConstraints(state, vname, idType, newV))
        follow(state2)
      case TypedAst.ReturnStmt(source, expr, assertions) =>
        debugPrint(s"Executing return statement in line ${source.getLine}")
        val returnv: SVal[SortValue] = ctxt.translateExprV(expr)

        val state2 = state.copy(
          invocationRes = state.invocationRes.put(state.currentInvocation, SReturnVal(ctxt.currentProcedure, returnv))
          // TODO update knownIds
        ).withTrace(s"Return ${returnv}", source)

        // check invariant in state2
        val ir = checkInvariant(source, ctxt, state2, s"After return in line ${source.getLine}.")
        ir.ifCounterExample(c => throw new SymbolicExecutionException(c))


        follow(state2.withInvariantResult(ir))
      case TypedAst.AssertStmt(source, expr) =>
        debugPrint(s"Executing assert statement in line ${source.getLine}")
        val assertFailed = NamedConstraint("assert_failed", 0,
          SNot(ctxt.translateExpr(expr)))
        ctxt.check(assertFailed :: state.pathConditions, s"assert-line-${source.getLine}", true) match {
          case SymbolicContext.Unsatisfiable(unsatCore) =>
            writeOutputFile(s"${ctxt.currentProcedure}_check_assert_${source.getLine}.unsatcore",
              s"""
                 |Check ok at assert statement in line ${source.getLine} using assertions:
                 |${unsatCore.map(_.description).mkString("\n")}
                 |""".stripMargin
            )


          // check ok
          case SymbolicContext.Unknown =>
            throwSymbolicCounterExample(
              s"Assertion in line ${source.getLine} might not hold.",
              source.range,
              state,
              None,
              ctxt
            )
          case s: Satisfiable =>
            throwSymbolicCounterExample(
              s"Assertion in line ${source.getLine} failed: $expr",
              source.range,
              state,
              Some(s.model),
              ctxt
            )
        }
        val state2 = state.withTrace(s"assert $expr", source)
          .withConstraint("assertion", ctxt.translateExpr(expr))
        follow(state2)
    }
  }

  def executeBeginAtomic(source: SourceTrace, state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.currentTransaction match {
      case Some(tx) =>
        throwSymbolicCounterExample(
          s"Already in a transaction",
          source.range,
          state,
          None,
          ctxt
        )
      case None =>
        // create variable for the new transaction
        val tx = ctxt.makeVariable[SortTxId]("tx")
        // state_monotonicGrowth i S S'
        val state2 = monotonicGrowth(state, ctxt)


        var newConstraints = mutable.ListBuffer[NamedConstraint]()

        // newTxns ⊆ dom (transactionOrigin S');
        val newTxns = SSetVar[SortTxId](ctxt.makeVariable("newTxns"))
        newConstraints += NamedConstraint("new_transactions_exist", 2,
          newTxns.isSubsetOf(MapDomain(state2.transactionOrigin))
        )


        // newCalls = callsInTransaction S' newTxns ↓ happensBefore S'
        val newCalls = SSetVar[SortCallId](ctxt.makeVariable("newCalls"))

        // TODO add restrictions to newCalls
        // vis' = vis ∪ newCalls
        val vis2 = SSetVar(ctxt.makeVariable[SortSet[SortCallId]]("vis"))
        newConstraints += NamedConstraint("vis_update", 1,
          SEq(vis2, SSetUnion(state2.visibleCalls, newCalls)))
        // TODO ⋀c. callOrigin S' c ≠ Some t


        val state3 = state2.copy(
          visibleCalls = vis2
        )

        // transactionStatus S t = None;
        // TODO check difference to Isabelle
        newConstraints += NamedConstraint(s"${tx.name}_fresh", 1,
          tx.invocation(state3).isNone)
        // ⋀t. transactionOrigin S t ≜ i ⟷ transactionOrigin S' t ≜ i; ― ‹No new transactions are added to current invocId.›
        val t = ctxt.makeBoundVariable[SortTxId]("t")
        newConstraints += NamedConstraint("no_new_transactions_added_to_current", 2,
          forall(t, (state.transactionOrigin.get(t) === SSome(state.currentInvocation))
            === (state3.transactionOrigin.get(t) === SSome(state.currentInvocation)))
        )
        // no new calls are added to current invocation:
        newConstraints += NamedConstraint("no_new_calls_addded_to_current", 2,
          SEq(
            state.invocationCalls.get(state.currentInvocation),
            state3.invocationCalls.get(state.currentInvocation)))
        // invariant_all S';
        newConstraints ++= invariant("at_transaction_begin", state3)(ctxt)
        // ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;


        newConstraints ++= assumeWellformed("transaction_begin", state3, ctxt)

        state3.copy(
          transactionOrigin = state2.transactionOrigin.put(tx, SSome(state2.currentInvocation)),
          currentTransaction = Some(tx)
        ).withTrace("Start of transaction", source)
          .withConstraints(newConstraints)
    }
  }


  def executeEndAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.copy(
      currentTransaction = None
    )
  }


  def throwSymbolicCounterExample(message: String, source: SourceRange, state: SymbolicState, model: Option[Model], ctxt: SymbolicContext): Nothing = {
    throw new SymbolicExecutionException(makeSymbolicCounterExample(message, source, state, model, ctxt))
  }


  def makeSymbolicCounterExample(message: String, source: SourceRange, state: SymbolicState, model: Option[Model],
    ctxt: SymbolicContext
  ): SymbolicCounterExample = {

    val name = s"${ctxt.currentProcedure}_line${source.start.line}"

    val translation: Translation = makeTranslation(name, state, ctxt)


    val traceWithModel: Trace[Option[SymbolicCounterExampleModel]] = model match {
      case Some(m) =>
        state.trace.mapInfo(s => Some(extractModel(s, m, prog)))
      case None =>
        state.trace.mapInfo(_ => None)
    }

    val emodel: Option[SymbolicCounterExampleModel] =
      model.map(m => extractModel(state, m, prog))


    SymbolicCounterExample(
      message = message,
      errorLocation = source,
      trace = traceWithModel,
      model = emodel,
      translation = translation
    )
  }

  private def makeTranslation(name: String, state: SymbolicState, ctxt: SymbolicContext) = {
    val isabelleTranslation: String = createIsabelleDefs(name, ctxt.datypeImpl, state.pathConditions)
    val cvcTranslation = ctxt.exportConstraints(state.pathConditions)
    val smtTranslation = ctxt.exportConstraintsToSmt(state.pathConditions)
    val translation = Translation(
      name = name,
      isabelleTranslation = isabelleTranslation,
      smtTranslation = smtTranslation,
      cvcTranslation = cvcTranslation
    )
    translation
  }


  //  private def isDefaultKey(c: SVal[_]): Boolean = c match {
  //    case SValOpaque("default_value", _, _) => true
  //    case _ => false
  //  }


  /** Checks the invariant in the given state.
   * Returns None if no problem was found and an error message otherwise.
   *
   * Implementation details:
   * The check is done by adding the negated invariant to the current context and checking if
   * the resulting context is unsatisfiable.
   *
   * If the context is satisfiable this means that there is a model such that all assumptions
   * in the current context are true, but the invariant is false.
   *
   * */
  private def checkInvariant(source: SourceTrace, ctxt: SymbolicContext, state: SymbolicState, where: String): CheckInvariantResult = {

    //    debugPrint("checkInvariant: before")
    //    ctxt.check() match {
    //      case Unsatisfiable =>
    //        // ok
    //        throw new RuntimeException("failed before invariant-check ...")
    //      case Unknown =>
    //        debugPrint("checkInvariant1: unknown ... ")
    //      //        throw new RuntimeException(s"")
    //      case s: Satisfiable =>
    //        debugPrint("checkInvariant1: Satisfiable ")
    //    }

    /**
     * checks if expr evaluates to result.
     * If not a counter example is provided.
     */
    def checkSVal(where: String, expr: SVal[SortBoolean], result: Boolean, state1: SymbolicState): CheckBooleanExprResult =
      expr match {
        case SNamedVal(name, value) =>
          checkSVal(s"$where-$name", value, result, state1)
        case SAnd(left, right) if result =>
          checkSVal(s"$where-left", left, true, state1).orElse(
            checkSVal(s"$where-right", left, true, state1))
        case _ =>


          val constraint =
            if (result) {
              NamedConstraint("invariant_not_violated", 0, SNot(expr))
            } else {
              NamedConstraint("invariant_not_violated", 0, expr)
            }
          val state = state1.withConstraints(List(constraint))

          val constraints = state.pathConditions


          //      debugPrint({
          val isabelleTranslation = createIsabelleDefs(s"${ctxt.currentProcedure}_$where", ctxt.datypeImpl, constraints)
          writeOutputFile(s"${ctxt.currentProcedure}_$where.thy", isabelleTranslation)


          val cvc4 = ctxt.exportConstraints(constraints)
          writeOutputFile(s"${ctxt.currentProcedure}_$where.cvc", cvc4)

          val smt = ctxt.exportConstraintsToSmt((constraints))

          val translation = Translation(where, isabelleTranslation, cvc4, smt)

          //        ""
          //      })

          val counterExample: Option[SymbolicCounterExample] = ctxt.check(constraints, s"inv-$where", true) match {
            case Unsatisfiable(unsatCore) =>
              debugPrint("checkInvariant: unsat, ok")

              writeOutputFile(s"${ctxt.currentProcedure}_$where.unsatcore",
                s"""
                   |Check ok $where using assertions:
                   |${unsatCore.map(_.description).mkString("\n")}
                 """.stripMargin
              )

              // ok
              None
            case Unknown =>
              debugPrint("checkInvariant: unknown")
              Some(makeSymbolicCounterExample(
                s"Could not prove invariant $where",
                source.range,
                state,
                None,
                ctxt
              ))
            case s: Satisfiable =>
              debugPrint("checkInvariant: satisfiable, computing model ...")
              val model = s.model
              val str = printModel(model, state).prettyStr(200)
              debugPrint(str)
              val msg =
                if (s.isIncomplete)
                  s"Invariant might not hold $where (counter example does not consider all constraints)"
                else
                  s"Invariant does not hold $where"
              Some(makeSymbolicCounterExample(
                msg,
                source.range,
                state,
                Some(model),
                ctxt
              ))
          }
          CheckBooleanExprResult(counterExample, List(translation))
      }

    def checkBooleanExpr(where: String, expr: InExpr, result: Boolean, qVars: Map[InVariable, SymbolicVariable[SymbolicSort]], state: SymbolicState): CheckBooleanExprResult = {
      expr match {
        case TypedAst.QuantifierExpr(_, Forall(), vs, body) if result =>
          val vars: Map[InVariable, SymbolicVariable[SymbolicSort]] = vs.map(v => v -> ctxt.makeVariable(v.name.name)(ExprTranslation.translateType(v.typ)(ctxt))).toMap

          val state2 = vars.foldLeft(state)((s, p) => s.withLocal(ProgramVariable(p._1.name.name), p._2))

          checkBooleanExpr(where, body, result, qVars ++ vars, state2)
        case ApplyBuiltin(_, _, BF_and(), List(l, r)) if result =>
          checkBooleanExpr(s"$where-l", l, result, qVars, state).orElse(
            checkBooleanExpr(s"$where-r", l, result, qVars, state))
        case _ =>
          checkSVal(where, ExprTranslation.translate(expr)(SymbolicSort.bool, ctxt, state), result, state).map((ce: SymbolicCounterExample) => {


            val locals: List[String] =
              for {
                model <- ce.model.toList
                ls <- model.state.localStates.values.toList
                v <- ls.varValues
              } yield s"${v._1.name} = ${v._2.value}"

            //            println(s"locals = $locals")
            //            ce.trace.steps.headOption match {
            //              case Some(value) =>
            //                value.info match {
            //                  case Some(model) =>
            //                    println(s"locals = $locals")
            //                  case None =>
            //                }
            //              case None =>
            //            }

            ce.copy(
              message = ce.message
                + s"\nInvariant in ${expr.getSource.range}: $expr"
                + s"\nWith variables: " + locals.mkString(", ")
            )
          })
      }
    }


    val results: LazyList[CheckBooleanExprResult] =
      for {
        (inv, i) <- prog.invariants.to(LazyList).zipWithIndex
        if !inv.isFree
      } yield checkBooleanExpr(s"${where}_inv_${inv.name}", inv.expr, true, Map(), state)

    CheckInvariantResult(results)
  }

  private def invariant(where: String, state: SymbolicState)(implicit ctxt: SymbolicContext): List[NamedConstraint] = {
    for ((inv, i) <- prog.invariants.zipWithIndex) yield {
      val cond = ExprTranslation.translate(inv.expr)(SymbolicSort.bool, ctxt, state)
      NamedConstraint(s"${where}_invariant_${inv.name}", inv.priority, cond)
    }
  }


  private def makeVariablesForParameters(ctxt: SymbolicContext, params: List[InVariable]): List[(ProgramVariable, SVal[SortValue])] = {
    for (p <- params) yield
      ProgramVariable(p.name.name) -> ctxt.makeVariable(s"${p.name}_init")(ctxt.translateSortVal(p.typ))
  }


}

case class SymbolicCounterExample(
  message: String,
  errorLocation: SourceRange,
  // trace including a model for the state after each step
  trace: Trace[Option[SymbolicCounterExampleModel]],
  model: Option[SymbolicCounterExampleModel],
  translation: Translation
)

case class SymbolicCounterExampleModel(
  state: Interpreter.State,
  modelText: Doc,
  renderResult: RenderResult
) {
  def toXml: Elem = {
    <counterExample>
      <modelText>
        {modelText.prettyStr(120)}
      </modelText>{renderResult.toXml}
    </counterExample>
  }

  override def toString: String = s"SymbolicCounterExampleModel($state)"
}

case class CheckInvariantResult(results: LazyList[CheckBooleanExprResult]) {
  def translations: List[Translation] =
    results.flatMap(_.translations).toList

  def ifCounterExample(f: SymbolicCounterExample => Unit): Unit = {
    firstCounterExample match {
      case Some(c) => f(c)
      case None =>
    }
  }

  def firstCounterExample: Option[SymbolicCounterExample] = {
    results.flatMap(r => r.counterExample).headOption
  }
}

case class CheckBooleanExprResult(
  counterExample: Option[SymbolicCounterExample],
  translations: List[Translation]
) {
  def hasCounterExample: Boolean = counterExample.isDefined

  def map(f: SymbolicCounterExample => SymbolicCounterExample): CheckBooleanExprResult =
    copy(counterExample = counterExample.map(f))

  def orElse(alternative: => CheckBooleanExprResult): CheckBooleanExprResult = {
    if (counterExample.isDefined) this
    else {
      val r = alternative
      CheckBooleanExprResult(
        r.counterExample,
        r.translations ++ translations
      )
    }
  }

}

object SymbolicEvaluator {
  def makeKnownIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicVariable[SortSet[SortCustomUninterpreted]]] = {
    idTypes.map(t => {
      val idType = IdType(t.name.name)()
      val sort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> ctxt.makeVariable[SortSet[SortCustomUninterpreted]](s"knownIds_${t.name}")(SortSet(sort))
    })
      .toMap
  }

  def makeGeneratedIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] = {
    idTypes.map(t => {
      val idType = IdType(t.name.name)()
      val keySort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> symbolicMapVar[SortCustomUninterpreted, SortOption[SortInvocationId]](s"generatedIds_${t.name}")(keySort, implicitly, implicitly)
    })
      .toMap
  }

  private def idTypes(implicit ctxt: SymbolicContext): List[TypedAst.InTypeDecl] =
    ctxt.prog.types.filter(_.isIdType)

}
