package crdtver.symbolic

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import crdtver.language.InputAst.BuiltInFunc.BF_and
import crdtver.language.InputAst.Forall
import crdtver.language.TypedAst._
import crdtver.language.{InvariantTransform, TypedAst}
import crdtver.symbolic.IsabelleTranslation.createIsabelleDefs
import crdtver.symbolic.SVal._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.SymbolicMapVar.symbolicMapVar
import crdtver.symbolic.SymbolicSort._
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, InvocationInfo, LocalVar, SnapshotTime, TransactionId, TransactionInfo}
import crdtver.testing.Visualization.RenderResult
import crdtver.testing.{Interpreter, Visualization}
import crdtver.utils.PrettyPrintDoc.Doc
import crdtver.utils.{MapWithDefault, StringBasedIdGenerator}

import scala.collection.{MapView, mutable}
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.xml.Elem

/** translation of the checked formulas */
case class Translation(
  name: String,
  isabelleTranslation: String,
  smtTranslation: String
) {
  val toXml: Elem = <translation name={name}>
    <isabelle>
      {isabelleTranslation}
    </isabelle>
    <smt>
      {smtTranslation}
    </smt>
  </translation>

}

case class SymbolicExecutionRes(
  proc: String,
  time: Duration,
  error: Option[SymbolicCounterExample],
  translations: List[Translation]
)

class SymbolicExecutionException(
  val counterExample: SymbolicCounterExample
) extends RuntimeException(counterExample.message) {
  override def toString: String = s"SymbolicExecutionError:\n$counterExample"
}

class SymbolicEvaluator(
  val originalProg: InProgram
) {

  val prog: InProgram = InvariantTransform.transformProg(originalProg)

  def checkProgram(): LazyList[SymbolicExecutionRes] = {
    debugPrint("checking program")
    for (proc <- prog.procedures.to(LazyList) if proc.name.name == "getMessage") yield checkProcedure(proc)
  }

  private def idTypes(): List[TypedAst.InTypeDecl] =
    prog.types.filter(_.isIdType)


  private def checkProcedure(proc: TypedAst.InProcedure): SymbolicExecutionRes = {
    val startTime = System.currentTimeMillis()
    try {
      debugPrint(s"checking procedure ${proc.name}")
      debugPrint(s"proc:\n${proc.printAst}")
      val z3Translation = new ToSmtTranslation()
      implicit val ctxt: SymbolicContext = new SymbolicContext(z3Translation, proc.name.name, prog)
      z3Translation.datatypeImpl = ctxt.datypeImpl

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
        visibleCalls = SSetEmpty(),
        trace = Trace(),
        snapshotAddition = SSetVar(ctxt.makeVariable("snapshotAddition")),
        translations = List()
      )

      var constraints = mutable.ListBuffer[NamedConstraint]()

      // there are a few restrictions we can assume for the initial state:
      // this follows the begin-invoc rule

      // >> invocationOp S i = None;
      constraints += NamedConstraint("i_fresh",
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
      constraints += NamedConstraint("no_transaction_in_new_invocation", {
        val tx = ctxt.makeVariable[SortTxId]("tx", true)
        forall(tx, state.transactionOrigin.get(tx) !== SSome(state.currentInvocation))
      })


      // there are no calls in the current invocation:
      constraints += NamedConstraint("no_call_in_new_invocation",
        state.invocationCalls.get(i) === SSetEmpty())

      constraints ++= assumeWellformed("before_procedure_invocation", state, ctxt)

      val args: List[SVal[SortValue]] = params.map(_._2)
      // >>   valid = invariant_all S'';  ― ‹  TODO check invariant in C ?  ›
      val invocationInfo: SVal[SortInvocationInfo] = SInvocationInfo(proc.name.name, args)

      val state2 = state.copy(
        invocationOp = state.invocationOp.put(i, invocationInfo),
        //SymbolicMapUpdated(i, invocationInfo, state.invocationOp)
      ).withConstraints(constraints.toList)
        .withTrace(s"Invocation of ${proc.name}(${args.mkString(", ")})", proc.getSource())

      // check the invariant in state2:
      val ir = checkInvariant(proc.getSource(), ctxt, state2, s"directly after invocation of ${proc.name}")
      ir.ifCounterExample(ce => throw new SymbolicExecutionException(ce))


      val state3 = state2.withInvariantResult(ir)
      //      match {
      //        case Some(msg) =>
      //          throw new SymbolicExecutionException(msg)
      //        case None =>
      //        // ok
      //      }

      // continue evaluating the procedure body:
      val finalState: SymbolicState = executeStatement(proc.body, state3, ctxt, (s, _) => s)
      SymbolicExecutionRes(
        proc.name.name,
        Duration.apply(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS),
        None,
        finalState.translations
      )
    } catch {
      case e: SymbolicExecutionException =>
        SymbolicExecutionRes(
          proc.name.name,
          Duration.apply(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS),
          Some(e.counterExample),
          List(e.counterExample.translation)
        )
    }


  }


  private def makeKnownIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicVariable[SortSet[SortCustomUninterpreted]]] = {
    idTypes().map(t => {
      val idType = IdType(t.name.name)()
      val sort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> ctxt.makeVariable[SortSet[SortCustomUninterpreted]](s"knownIds_${t.name}")(SortSet(sort))
    })
      .toMap
  }

  private def makeGeneratedIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] = {
    idTypes().map(t => {
      val idType = IdType(t.name.name)()
      val keySort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> symbolicMapVar[SortCustomUninterpreted, SortOption[SortInvocationId]](s"generatedIds_${t.name}")(keySort, implicitly, implicitly)
    })
      .toMap
  }

  def executeStatements(stmts: List[TypedAst.InStatement], state: SymbolicState, ctxt: SymbolicContext, follow: (SymbolicState, SymbolicContext) => SymbolicState): SymbolicState = stmts match {
    case Nil =>
      follow(state, ctxt)
    case x :: xs =>
      executeStatement(x, state, ctxt, executeStatements(xs, _, _, follow))
  }


  def debugPrint(str: => String): Unit = {
    //            println(str)
  }

  def newIdConstraints(state: SymbolicState, vname: String, idType: IdType, newV: SVal[SortCustomUninterpreted]): Iterable[NamedConstraint] = {
    val result = mutable.ListBuffer[NamedConstraint]()
    result += NamedConstraint(s"${vname}_new_id_fresh",
      state.generatedIds(idType)(newV).isNone)

    result
  }

  private def executeStatement(stmt: TypedAst.InStatement, state: SymbolicState, ctxt: SymbolicContext, follow: (SymbolicState, SymbolicContext) => SymbolicState): SymbolicState = {
    implicit val istate: SymbolicState = state
    stmt match {
      case TypedAst.BlockStmt(source, stmts) =>
        debugPrint(s"Executing block in line ${source.getLine}")
        executeStatements(stmts, state, ctxt, follow)
      case TypedAst.Atomic(source, body) =>
        debugPrint(s"Executing begin-atomic in line ${source.getLine}")
        val state2 = executeBeginAtomic(source, state, ctxt)
        executeStatement(body, state2, ctxt, (state3, ctxt) => {
          debugPrint(s"Executing end-atomic in line ${source.range.stop.line}")
          val state4 = executeEndAtomic(state3, ctxt)

          // assume state 4 wellformed
          assumeWellformed("after_transaction", state4, ctxt)
          // check invariant in state4
          val ir = checkInvariant(source, ctxt, state4, s"When committing transaction of atomic block in line ${source.getLine}.")
          ir.ifCounterExample(c => throw new SymbolicExecutionException(c))

          follow(state4.withInvariantResult(ir), ctxt)
        })
      case TypedAst.LocalVar(source, variable) =>
        debugPrint(s"Executing local variable $variable in line ${source.getLine}")
        // nothing to do
        state.withTrace(s"Local variable $variable", variable.getSource())
      case TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
        debugPrint(s"Executing if-statement in line ${source.getLine}")
        val condV: SVal[SortBoolean] = ExprTranslation.translate(cond)(bool, ctxt, state)

        // first assume the condition is true
        val ifTrueState = state.withConstraint("if_statement_condition_true", condV)
        ctxt.check(ifTrueState.constraints) match {
          case Unsatisfiable =>
          // then-branch cannot be taken
          case Unknown | _: Satisfiable =>
            debugPrint(s"Executing then-statement in line ${thenStmt.getSource().getLine}")
            val state2 = ifTrueState.withTrace(s"Executing then-statement", thenStmt)
            executeStatement(thenStmt, state2, ctxt, follow)
        }

        // next assume the condition is false:
        debugPrint(s"Executing else-statement in line ${elseStmt.getSource().getLine}")
        val ifFalseState = state.withConstraint("if_statement_condition_false", SNot(condV))
        ctxt.check(ifFalseState.constraints) match {
          case Unsatisfiable =>
            // else-branch cannot be taken
            ifFalseState.copy(satisfiable = false)
          case Unknown | _: Satisfiable =>
            val state2 = ifFalseState.withTrace("Executing else-statement", elseStmt)
            executeStatement(elseStmt, state2, ctxt, follow)
        }
      case TypedAst.MatchStmt(source, expr, cases) =>
        // TODO
        ???
      case TypedAst.CrdtCall(source, call) =>
        debugPrint(s"Executing CRDT call in line ${source.getLine}")
        val t: SVal[SortTxId] = state.currentTransaction.get
        val c: SymbolicVariable[SortCallId] = ctxt.makeVariable("c" + state.currentCallIds.size)
        // assume new call c is distinct from all others:
        val newConstraints = List(
          NamedConstraint(s"${c.name}_freshA", SDistinct(c :: state.currentCallIds)),
          NamedConstraint(s"${c.name}_freshB", SEq(state.calls.get(c), SCallInfoNone()))
        )

        // TODO maybe choose type based on query type
        // TODO assume query specification for res

        val args: List[SVal[SymbolicSort]] = call.args.map(a => ExprTranslation.translateUntyped(a)(ctxt, state))
        val callInfo: SVal[SortCall] = SCallInfo(call.functionName.name, args)

        val newCurrentCallIds = state.currentCallIds :+ c

        val newVis = state.visibleCalls + c
        val state2 = state.copy(
          calls = SymbolicMapVar(SNamedVal("calls", state.calls.put(c, callInfo))),
          currentCallIds = newCurrentCallIds,
          callOrigin = SymbolicMapVar(SNamedVal("callOrigin", state.callOrigin.put(c, SSome(t)))),
          visibleCalls = SSetVar(SNamedVal("vis", newVis)),
          happensBefore = SymbolicMapVar(SNamedVal("happensBefore", state.happensBefore.put(c, newVis))),
          invocationCalls = state.invocationCalls.put(state.currentInvocation, SVal.makeSet(newCurrentCallIds))
        ).withTrace(s"call ${call.functionName.name}(${args.mkString(", ")})", source)
          .withConstraints(newConstraints)

        follow(state2, ctxt)
      case TypedAst.Assignment(source, varname, expr) =>
        debugPrint(s"Executing assignment in line ${source.getLine}: ${stmt.printAst}")
        // use a new variable here to avoid duplication of expressions
        val v = ctxt.makeVariable(varname.name)(ctxt.translateSortVal(expr.getTyp))
        val state2 = state.copy(
          localState = state.localState + (ProgramVariable(varname.name) -> v)
        ).withTrace(s"assignment $varname", source)
          .withConstraint(s"${v.name}_assignment",
            v === ctxt.translateExprV(expr))
        follow(state2, ctxt)
      case TypedAst.NewIdStmt(source, varname, typename) =>
        debugPrint(s"Executing new-id statement in line ${source.getLine}")
        val idType = typename.asInstanceOf[IdType]
        val vname = varname.name
        val newV: SVal[SortCustomUninterpreted] = ctxt.makeVariable(vname)(ctxt.translateSort(typename)).asInstanceOf[SVal[SortCustomUninterpreted]]
        val state2 = state.copy(
          localState = state.localState + (ProgramVariable(vname) -> newV)
        ).withTrace(s"New-id $varname", source)
          .withConstraints(newIdConstraints(state, vname, idType, newV))
        follow(state2, ctxt)
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


        follow(state2.withInvariantResult(ir), ctxt)
      case TypedAst.AssertStmt(source, expr) =>
        debugPrint(s"Executing assert statement in line ${source.getLine}")
        val assertFailed = NamedConstraint("assert_failed",
          SNot(ctxt.translateExpr(expr)))
        ctxt.check(assertFailed :: state.constraints) match {
          case SymbolicContext.Unsatisfiable =>
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
        follow(state2, ctxt)
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
        newConstraints += NamedConstraint("new_transactions_exist",
          newTxns.isSubsetOf(MapDomain(state2.transactionOrigin))
        )


        // newCalls = callsInTransaction S' newTxns ↓ happensBefore S'
        val newCalls = SSetVar[SortCallId](ctxt.makeVariable("newCalls"))

        // TODO add restrictions to newCalls
        // vis' = vis ∪ newCalls
        val vis2 = SSetVar(ctxt.makeVariable[SortSet[SortCallId]]("vis"))
        newConstraints += NamedConstraint("vis_update",
          SEq(vis2, SSetUnion(state2.visibleCalls, newCalls)))
        // TODO ⋀c. callOrigin S' c ≠ Some t


        val state3 = state2.copy(
          visibleCalls = vis2
        )

        // transactionStatus S t = None;
        // TODO check difference to Isabelle
        newConstraints += NamedConstraint(s"${tx.name}_fresh",
          tx.invocation(state3).isNone)
        // ⋀t. transactionOrigin S t ≜ i ⟷ transactionOrigin S' t ≜ i; ― ‹No new transactions are added to current invocId.›
        val t = ctxt.makeBoundVariable[SortTxId]("t")
        newConstraints += NamedConstraint("no_new_transactions_added_to_current",
          forall(t, (state.transactionOrigin.get(t) === SSome(state.currentInvocation))
            === (state3.transactionOrigin.get(t) === SSome(state.currentInvocation)))
        )
        // no new calls are added to current invocation:
        newConstraints += NamedConstraint("no_new_calls_addded_to_current",
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

  /** *
    * Adds assumptions that the given state is well-formed.
    */
  def assumeWellformed(where: String, state: SymbolicState, ctxt: SymbolicContext): Iterable[NamedConstraint] = {
    val constraints = mutable.ListBuffer[NamedConstraint]()

    implicit val implicitState = state

    // no happensBefore relation between non-existing calls
    constraints += NamedConstraint("happensBefore_exists_l", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1.op === SCallInfoNone())
          --> !(c1 happensBefore c2)
      )
    })
    constraints += NamedConstraint("happensBefore_exists_r", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1.op === SCallInfoNone())
          --> !(c1 happensBefore c2)
      )
    })

    // happens-before relation between calls in the same invocation
    constraints += NamedConstraint("invocation_sequential", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      val tx1 = ctxt.makeBoundVariable[SortTxId]("tx1")
      val tx2 = ctxt.makeBoundVariable[SortTxId]("tx2")
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      forallL(List(c1, tx1, i, c2, tx2),
        ((c1.tx === SSome(tx1))
          && (tx1.invocation === SSome(i))
          && (c2.tx === SSome(tx2))
          && (tx2.invocation === SSome(i)))
          --> ((c1 happensBefore c2) || (c2 happensBefore c1))
      )
    })


    // visible calls are a subset of all calls
    constraints += NamedConstraint("visibleCalls_exist", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c,
        c.isVisible
          --> (c.op !== SCallInfoNone())
      )
    })

    // visible calls forms consistent snapshot
    constraints += NamedConstraint("visibleCalls_transaction_consistent1", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1.isVisible
          && (c1 inSameTransactionAs c2)
          && (c2.op !== SCallInfoNone()))
          --> c2.isVisible
      )
    })

    constraints += NamedConstraint("visibleCalls_causally_consistent", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c2.isVisible
          && (c1 happensBefore c2))
          --> c1.isVisible
      )
    })


    // happensBefore is a partial order (reflexivity, transitivity, antisymmetric)
    constraints += NamedConstraint("happensBefore_reflex", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c,
        (c.op !== SCallInfoNone()) --> (c happensBefore c)
      )
    })
    constraints += NamedConstraint("happensBefore_trans", {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      val z = ctxt.makeBoundVariable[SortCallId]("z")
      forallL(List(x, y, z),
        ((x happensBefore y) && (y happensBefore z)) --> (x happensBefore y)
      )
    })

    constraints += NamedConstraint("happensBefore_antisym", {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      forallL(List(x, y),
        ((x happensBefore y) && (y happensBefore x)) --> (x === y)
      )
    })


    // no invocation implies no result
    constraints += NamedConstraint("no_invocation_implies_no_result", {
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      forallL(List(i),
        (i.op === SInvocationInfoNone()) --> (i.res === SReturnValNone())
      )
    })

    // transaction consistency with happens before:
    constraints += NamedConstraint("no_invocation_implies_no_result", {
      val x1 = ctxt.makeBoundVariable[SortCallId]("x1")
      val x2 = ctxt.makeBoundVariable[SortCallId]("x2")
      val y1 = ctxt.makeBoundVariable[SortCallId]("y1")
      val y2 = ctxt.makeBoundVariable[SortCallId]("y2")
      forallL(List(x1, x2, y1, y2),
        ((x1 inSameTransactionAs x2)
          && (y1 inSameTransactionAs y2)
          && !(x1 inSameTransactionAs y1)
          && (x2 happensBefore y1))
          --> (x2 happensBefore y2)
      )
    })



    // TODO not needed because of different encoding?:
    // invocation happens-before of origins implies happens-before of calls
    // no result implies not in invocation happens before
    // in happens before implies not NoResult


    // old ....


    // definition of invocationCalls
    constraints += NamedConstraint("WF_invocationCalls", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(i, forall(c,
        i.calls.contains(c) <-->
          exists(tx,
            (c.tx === SSome(tx))
              && (tx.invocation === SSome(i)))))
    })



    // domain calls = domain callsOrigin
    constraints += NamedConstraint("WF_callOrigin", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")

      forall(c,
        c.tx.isNone <-->
          (c.op === SCallInfoNone()))
    })




    // when the transaction invocation is none, then there can be no calls in the transaction
    constraints += NamedConstraint("WF_transactionOrigin_callOrigin", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")
      forall(tx,
        tx.invocation.isNone -->
          forall(c, c.tx !== SSome(tx)))
    })


    constraints += NamedConstraint("WF_no_call_implies_no_happensBefore", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")

      forall(c,
        c.tx.isNone -->
          (c.happensBeforeSet === SSetEmpty[SortCallId]()))
    })


    constraints += NamedConstraint("WF_no_call_implies_not_in_happensBefore", {
      val ca = ctxt.makeBoundVariable[SortCallId]("ca")
      val cb = ctxt.makeBoundVariable[SortCallId]("cb")

      forall(ca, forall(cb,
        ca.tx.isNone -->
          !(ca happensBefore cb)))
    })

    // callOrigin exists
    constraints += NamedConstraint("WF_callOrigin_exists", {
      val ca = ctxt.makeBoundVariable[SortCallId]("ca")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(ca, forall(tx,
        (ca.tx === SSome(tx)) -->
          !tx.invocation.isNone))
    })


    // transactionOrigin exists
    constraints += NamedConstraint("WF_transactionOrigin_exists", {
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(tx, forall(i,
        (tx.invocation === SSome(i)) -->
          (i.op !== SInvocationInfoNone())))
    })



    // all parameters of method invocations are known ids
    for (proc <- prog.procedures) {
      for ((arg, argI) <- proc.params.zipWithIndex) {
        arg.typ match {
          case t: IdType =>
            val i = ctxt.makeBoundVariable[SortInvocationId]("i")
            val argVariables: List[SymbolicVariable[SortValue]] = proc.params.map(p => ctxt.makeBoundVariable[SortValue](p.name.name)(ExprTranslation.translateType(p.typ)(ctxt).asInstanceOf[SortValue]))
            val knownIds: SVal[SortSet[SortCustomUninterpreted]] = state.knownIds(t)
            constraints += NamedConstraint(s"${proc.name.name}_parameter_${arg.name}_known",
              forallL(i :: argVariables,
                (i.op === SInvocationInfo(proc.name.name, argVariables)) -->
                  knownIds.contains(argVariables(argI).asInstanceOf[SVal[SortCustomUninterpreted]]))
            )
          case _ =>
          // should also handle nested ids
        }
      }
    }



    // all returned values of method invocations are known ids
    for (proc <- prog.procedures) {
      proc.returnType match {
        case Some(returnType) =>
          returnType match {

            case t: IdType =>
              val i = ctxt.makeBoundVariable[SortInvocationId]("i")
              val r = ctxt.makeBoundVariable("result")(ExprTranslation.translateType(t)(ctxt).asInstanceOf[SortCustomUninterpreted])
              val knownIds: SVal[SortSet[SortCustomUninterpreted]] = state.knownIds(t)
              constraints += NamedConstraint(s"${proc.name.name}_result_known",
                forallL(List(i, r),
                  (i.res === SReturnVal(proc.name.name, r.upcast)) -->
                    knownIds.contains(r))
              )
            case _ =>
            // should also handle nested ids
          }
        case None =>
      }
    }

    // all parameters of database calls are generated
    for (operation <- prog.programCrdt.operations()) {
      for ((arg, argI) <- operation.params.zipWithIndex) {
        arg.typ match {
          case t: IdType =>
            val c = ctxt.makeBoundVariable[SortCallId]("c")
            val argVariables: List[SymbolicVariable[SymbolicSort]] = operation.params.map(p => ctxt.makeBoundVariable[SymbolicSort](p.name)(ExprTranslation.translateType(p.typ)(ctxt).asInstanceOf[SymbolicSort]))
            val generatedIds: SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]] = state.generatedIds(t)
            constraints += NamedConstraint(s"${operation.name}_call_parameter_${arg.name}_generated",
              forallL(c :: argVariables,
                (c.op === SCallInfo(operation.name, argVariables)) -->
                  !generatedIds.get(argVariables(argI).asInstanceOf[SVal[SortCustomUninterpreted]]).isNone)
            )
          case _ =>
          // should also handle nested ids
        }
      }
    }


    // if an id is known it was generated
    for ((t, knownIds) <- state.knownIds) {
      val x = ctxt.makeBoundVariable[SortCustomUninterpreted]("x")(SortCustomUninterpreted(t.name))
      constraints += NamedConstraint(s"${t.name}_knownIds_are_generated",
        forall(x,
          knownIds.contains(x) -->
            !state.generatedIds(t).get(x).isNone)
      )
    }


    // snapshotAddition is a subset of all calls
    constraints += NamedConstraint("snapshot_addition_subset_calls", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c, state.snapshotAddition.contains(c) --> (c.op !== SCallInfoNone()))
    })
    // snapshotAddition is transaction consistent
    constraints += NamedConstraint("snapshot_addition_transaction_consistent", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (state.snapshotAddition.contains(c1) && (c1.tx === c2.tx)) --> state.snapshotAddition.contains(c2))
    })
    // snapshotAddition is causally consistent
    constraints += NamedConstraint("snapshot_addition_transaction_consistent", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (state.snapshotAddition.contains(c1) && (c2 happensBefore c1)) --> state.snapshotAddition.contains(c2))
    })


    // TODO compare with WhyTranslation.wellformedConditions

    constraints.map(c => c.copy(description = s"${where}_${c.description}")).toList
  }

  def monotonicGrowth(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    implicit val ictxt: SymbolicContext = ctxt
    // create new variables for new state
    val state2 = state.copy(
      calls = symbolicMapVar("calls"),
      happensBefore = symbolicMapVar("happensBefore"),
      callOrigin = symbolicMapVar("callOrigin"),
      transactionOrigin = symbolicMapVar("transactionOrigin"),
      invocationCalls = symbolicMapVar("invocationCalls"),
      generatedIds = makeGeneratedIdsVar,
      knownIds = makeKnownIdsVar,
      snapshotAddition = SSetVar(ctxt.makeVariable("snapshotAddition"))
    )
    val constraints = mutable.ListBuffer[NamedConstraint]()


    // call origin growths:
    constraints += NamedConstraint("growth_callOrigin", {
      val c = ctxt.makeVariable[SortCallId]("c")
      val tx = ctxt.makeVariable[SortTxId]("tx")
      forall(c, forall(tx,
        (c.tx(state) === SSome(tx))
          --> (c.tx(state2) === SSome(tx))))
    })

    // monotonic growth of visible calls
    constraints += NamedConstraint("growth_visible_calls", {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        c.isVisible(state) --> c.isVisible(state2))
    })


    // monotonic growth of call ops
    constraints += NamedConstraint("growth_calls", {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.op(state2) === c.op(state)))
    })


    // monotonic growth of happensbefore
    // --> no new calls can be added before:
    constraints += NamedConstraint("growth_happensbefore", {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.happensBeforeSet(state2) === c.happensBeforeSet(state)))
    })

    // monotonic growth of call transaction
    constraints += NamedConstraint("growth_call_tx", {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.tx(state2) === c.tx(state)))
    })


    // monotonic growth of transaction origin
    constraints += NamedConstraint("growth_tx_origin", {
      val tx = ctxt.makeVariable[SortTxId]("tx")
      forall(tx,
        !tx.invocation(state).isNone --> (tx.invocation(state2) === tx.invocation(state)))
    })


    // monotonic growth of invocations
    constraints += NamedConstraint("growth_invocation_op", {
      val i = ctxt.makeVariable[SortInvocationId]("i")
      forall(i,
        (i.op(state) !== SInvocationInfoNone()) --> (i.op(state2) === i.op(state)))
    })

    // monotonic growth of invocationResult
    constraints += NamedConstraint("growth_invocation_res", {
      val i = ctxt.makeVariable[SortInvocationId]("i")
      forall(i,
        (i.res(state) !== SReturnValNone()) --> (i.res(state2) === i.res(state)))
    })

    // no new calls added to existing transactions:
    constraints += NamedConstraint("old_transactions_unchanged", {
      val tx = ctxt.makeVariable[SortTxId]("tx")
      val c = ctxt.makeVariable[SortCallId]("c")
      forallL(List(c, tx),
        ((c.op(state) === SCallInfoNone())
          && (c.op(state2) !== SCallInfoNone())
          && (c.tx(state2) === SSome(tx)))
          --> tx.invocation(state).isNone)
    })


    state2.withConstraints(constraints)
  }

  def executeEndAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.copy(
      currentTransaction = None
    )
  }


  def printModel(model: Model, state: SymbolicState): Doc = {
    import crdtver.utils.PrettyPrintDoc._

    implicit def exprToDoc(e: SVal[_]): Doc = e.toString

    implicit def mapToDoc[K, V](m: Map[K, V]): Doc =
      nested(2, line <> sep(line, m.map((e: (K, V)) => e._1.toString <+> "->" <+> e._2.toString).toList))

    implicit def mapViewToDoc[K, V](m: MapView[K, V]): Doc =
      nested(2, line <> sep(line, m.map((e: (K, V)) => e._1.toString <+> "->" <+> e._2.toString).toList))

    implicit def setToDoc[T](m: Set[T]): Doc =
      "{" <> sep(", ", m.map("" <> _.toString).toList) <> "}"

    state.trace.toString </>
      "calls = " <> extractMap(model.evaluate(state.calls)) </>
      "happensBefore = " <> extractMap(model.evaluate(state.happensBefore)).view.mapValues(extractSet) </>
      "callOrigin = " <> extractMap(model.evaluate(state.callOrigin)) </>
      "transactionOrigin = " <> extractMap(model.evaluate(state.transactionOrigin)) </>
      //      "generatedIds = " <> model.evaluate(state.generatedIds) </>
      //      "knownIds = " <> model.evaluate(state.knownIds) </>
      "invocationCalls = " <> extractMap(model.evaluate(state.invocationCalls)).view.mapValues(extractSet) </>
      "invocationOp = " <> extractMap(model.evaluate(state.invocationOp)) </>
      "invocationRes = " <> extractMap(model.evaluate(state.invocationRes)) </>
      "currentInvocation = " <> model.evaluate(state.currentInvocation) </>
      "currentTransaction = " <> state.currentTransaction.map(v => model.evaluate(v)).toString </>
      "localState = " <> sep(line, state.localState.toList.map { case (k, v) => k.name <+> ":=" <+> model.evaluate(v) }) </>
      "visibleCalls = " <> extractSet(model.evaluate(state.visibleCalls)) </>
      "currentCallIds = " <> sep(", ", state.currentCallIds.map("" <> model.evaluate(_))) </>
      "satisfiable = " <> state.satisfiable.toString
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
        state.trace.mapInfo(s => Some(extractModel(s, m)))
      case None =>
        state.trace.mapInfo(_ => None)
    }

    val emodel: Option[SymbolicCounterExampleModel] =
      model.map(m => extractModel(state, m))


    SymbolicCounterExample(
      message = message,
      errorLocation = source,
      trace = traceWithModel,
      model = emodel,
      translation = translation
    )
  }

  private def makeTranslation(name: String, state: SymbolicState, ctxt: SymbolicContext) = {
    val isabelleTranslation: String = createIsabelleDefs(name, ctxt.datypeImpl, state.constraints)
    val smtTranslation = ctxt.exportConstraints(state.constraints)
    val translation = Translation(
      name = name,
      isabelleTranslation = isabelleTranslation,
      smtTranslation = smtTranslation
    )
    translation
  }

  private def printInterpreterState(state: Interpreter.State): Doc = {
    import crdtver.utils.PrettyPrintDoc._

    //    calls: Map[CallId, CallInfo] = Map(),
    //        //    transactionCalls: Map[TransactionId, Set[CallId]] = Map(),
    //        maxCallId: Int = 0,
    //        transactions: Map[TransactionId, TransactionInfo] = Map(),
    //        maxTransactionId: Int = 0,
    //        invocations: Map[InvocationId, InvocationInfo] = Map(),
    //        maxInvocationId: Int = 0,
    //        // returned Ids for each id-type and the invocation that returned it
    //        knownIds: Map[IdType, Map[AnyValue, InvocationId]] = Map(),
    //        localStates: Map[InvocationId, LocalState] = Map()

    "calls: " <> nested(2, line <> sep(line, state.calls.values.map(c =>
      c.id.toString <+> "->" <+> c.operation.toString <>
        nested(2, line <> "clock = " <> c.callClock.toString </>
          "tx = " <> c.callTransaction.toString </>
          "invoc = " <> c.origin.toString)
    ))) </>
      "invocations: " <> nested(2, line <> sep(line, state.invocations.values.map(i =>
      i.id.toString <+> "->" <+> i.operation.toString <+> ", returned " <+> i.result.toString))) </>
      "transactions: " <> nested(2, line <> sep(line, state.transactions.values.map(tx =>
      tx.id.toString <+> "->" <+> tx.currentCalls.toString <+> " from " <+> tx.origin.toString)))
  }

  private def extractModel(state: SymbolicState, model: Model): SymbolicCounterExampleModel = {

    val iState: Interpreter.State = extractInterpreterState(state, model)

    debugPrint(s"STATE =\n$iState")

    val modelText: Doc = printModel(model, state) +
      "\n\nInterpreted state:\n----------------\n\n" +
      printInterpreterState(iState)

    val renderResult = Visualization.renderStateGraph(prog, iState)
    SymbolicCounterExampleModel(
      iState,
      modelText,
      renderResult
    )
  }

  def intersection[T](list: List[Set[T]]): Set[T] = list match {
    case List() => Set()
    case List(s) => s
    case x :: xs =>
      x.intersect(intersection(xs))
  }

  private def extractMap[K <: SymbolicSort, V <: SymbolicSort](cs: SVal[SortMap[K, V]]): Map[SVal[K], SVal[V]] = cs match {
    case SymbolicMapUpdated(k, v, b) =>
      extractMap(b) + (k.cast[K] -> v.cast[V])
    case m@SymbolicMapEmpty(dv) =>
      debugPrint(s"Empty with default $dv")
      dv match {
        case SNone(_) =>
          Map()
        case d =>
          new MapWithDefault(Map(), d)
      }
    case x =>
      throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
  }

  private def extractSet[T <: SymbolicSort](s: SVal[SortSet[T]]): Set[SVal[T]] = s match {
    case SSetInsert(base, v) =>
      extractSet(base) ++ v
    case SSetEmpty() =>
      Set()
    case SSetUnion(a, b) =>
      extractSet(a) ++ extractSet(b)
    case x =>
      throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
  }

  private def extractBool(s: SVal[SortBoolean]): Boolean = s match {
    case SBool(value) => value
    case x =>
      throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
  }

  //  private def isDefaultKey(c: SVal[_]): Boolean = c match {
  //    case SValOpaque("default_value", _, _) => true
  //    case _ => false
  //  }

  private def extractInterpreterState(state: SymbolicState, model: Model): Interpreter.State = {

    debugPrint(s"callsS = ${model.evaluate(state.calls)}")

    val translateCallId: StringBasedIdGenerator[SVal[SortCallId], CallId] =
      new StringBasedIdGenerator(CallId(_))

    val translateTransactionId: StringBasedIdGenerator[SVal[SortTxId], TransactionId] =
      new StringBasedIdGenerator(TransactionId(_))

    val translateInvocationId: StringBasedIdGenerator[SVal[SortInvocationId], InvocationId] =
      new StringBasedIdGenerator(InvocationId(_))

    def getSnapshotTime(c: SVal[SortCallId]): SnapshotTime = {
      val hb = model.evaluate(state.happensBefore.get(c))
      SnapshotTime(extractSet(hb).map(translateCallId))
    }

    def getCallTransaction(c: SVal[SortCallId]): TransactionId = {
      val to: SVal[SortOption[SortTxId]] = model.evaluate(state.callOrigin.get(c))
      to match {
        case SNone(ty) =>
          debugPrint(s"!!! Call $c is not in a transaction")
          TransactionId(-1)
        case SSome(t) =>
          translateTransactionId(t)
        case _ => ???
      }
    }

    def getTransactionOrigin(tx: SVal[SortTxId]): InvocationId = {
      val io: SVal[SortOption[SortInvocationId]] = model.evaluate(state.transactionOrigin.get(tx))
      io match {
        case SNone(ty) =>
          debugPrint(s"!!! Transaction $tx which is not in an invocation")
          InvocationId(-2)
        case SSome(i) =>
          translateInvocationId(i)
        case _ => ???
      }
    }

    def getCallInvocation(c: SVal[SortCallId]): InvocationId = {
      val to: SVal[SortOption[SortTxId]] = model.evaluate(state.callOrigin.get(c))
      to match {
        case SNone(ty) =>
          debugPrint(s"!!! Call $c is not in a transaction")
          InvocationId(-1)
        case SSome(tx) =>
          getTransactionOrigin(tx)
        case _ => ???
      }
    }

    def translateCall(value: SVal[SortCall]): DataTypeValue = value match {
      case SCallInfo(o, args) =>
        DataTypeValue(o, args.map(AnyValue(_)))
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }

    def translateInvocationOp(value: SVal[SortInvocationInfo]): DataTypeValue = value match {
      case SInvocationInfo(procName, args) =>
        DataTypeValue(procName, args.map(AnyValue(_)))
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }

    def translateInvocationRes(value: SVal[SortInvocationRes]): Option[DataTypeValue] = value match {
      case SReturnVal(m, v) =>
        Some(DataTypeValue(m, List(AnyValue(v))))
      case SReturnValNone() =>
        None
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }


    val scalls: Map[SVal[SortCallId], SVal[SortCall]] = extractMap(model.evaluate(state.calls))
    val sInvocationOp: Map[SVal[SortInvocationId], SVal[SortInvocationInfo]] = extractMap(model.evaluate(state.invocationOp))
    val sInvocationRes: Map[SVal[SortInvocationId], SVal[SortInvocationRes]] = extractMap(model.evaluate(state.invocationRes))

    // TODO replace this with tree traversal collecting all occurrences
    for (c <- scalls.keys) {
      translateCallId(c)
    }


    for (i <- sInvocationOp.keys) {
      translateInvocationId(i)
    }
    for (i <- sInvocationRes.keys) {
      translateInvocationId(i)
    }
    translateInvocationId(state.currentInvocation)
    for ((ci, i) <- translateInvocationId) {
      val iCalls: Set[SVal[SortCallId]] = extractSet(model.evaluate(state.invocationCalls.get(ci)))
      for (c <- iCalls)
        translateCallId(c)
    }

    translateCallId.freeze()
    translateInvocationId.freeze()


    val invocationCalls: Map[InvocationId, Set[CallId]] =
      (for ((si, i) <- translateInvocationId) yield {
        val cs = extractSet(model.evaluate(state.invocationCalls.get(si)))
        i -> cs.map(translateCallId)
      }).toMap


    val calls: Map[CallId, CallInfo] =
      (for ((sc, c) <- translateCallId) yield {
        val tx = getCallTransaction(sc)
        val invoc = getCallInvocation(sc)

        c -> CallInfo(
          c,
          translateCall(scalls(sc)),
          getSnapshotTime(sc),
          tx,
          invoc,
        )
      }).toMap

    debugPrint(s"calls = $calls")

    //    for ((i,cs) <-invocationCalls) {
    //      for (c <- cs) {
    //        calls.get(c) match {
    //          case Some(ci) =>
    //            if (ci.origin != i)
    //              throw new RuntimeException(s"Call $c from ${ci.callTransaction}/${ci.origin} should not appear in $i\n$invocationCalls")
    //          case None =>
    //            throw new RuntimeException(s"Could not find call info for $c in $i")
    //        }
    //      }
    //    }


    val transactions: Map[TransactionId, TransactionInfo] =
      (for ((st, t) <- translateTransactionId) yield {
        val callsInT: List[CallInfo] = calls.values.filter(_.callTransaction == t).toList
        t -> TransactionInfo(
          t,
          SnapshotTime(intersection(callsInT
            .map(_.callClock.snapshot))),
          getTransactionOrigin(st),
          callsInT,
          true
        )
      }).toMap

    debugPrint(s"transactions = $transactions")

    val invocations: Map[InvocationId, InvocationInfo] =
      (for ((si, i) <- translateInvocationId) yield {
        val invocationOp: SVal[SortInvocationInfo] = model.evaluate(state.invocationOp.get(si))
        val invocationRes: SVal[SortInvocationRes] = model.evaluate(state.invocationRes.get(si))
        i -> InvocationInfo(
          i,
          translateInvocationOp(invocationOp),
          translateInvocationRes(invocationRes)
        )
      }).toMap

    debugPrint(s"invocations = $invocations")

    val currentInvoc: InvocationId = translateInvocationId(state.currentInvocation)

    val varValues: Map[LocalVar, AnyValue] =
      for ((v, x) <- state.localState) yield
        LocalVar(v.name) -> AnyValue(model.evaluate(x))


    val localState = Interpreter.LocalState(
      varValues,
      List(), Interpreter.WaitForNothing(), None, Set()
    )

    Interpreter.State(
      calls = calls,
      transactions = transactions,
      invocations = invocations,
      localStates = Map(currentInvoc -> localState)
    )
  }


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
    def checkSVal(where: String, expr: SVal[SortBoolean], result: Boolean, state1: SymbolicState): CheckBooleanExprResult = {
      expr match {
        case SNamedVal(name, value) =>
          checkSVal(s"$where-$name", value, result, state1)
        case SAnd(left, right) if result =>
          checkSVal(s"$where-left", left, true, state1).orElse(
            checkSVal(s"$where-right", left, true, state1))
        case _ =>


          val constraint =
            if (result) {
              NamedConstraint("invariant_not_violated", SNot(expr))
            } else {
              NamedConstraint("invariant_not_violated", expr)
            }
          val state = state1.withConstraints(List(constraint))

          val constraints = state.constraints


          //      debugPrint({
          val isabelleTranslation = createIsabelleDefs(s"${ctxt.currentProcedure}_$where", ctxt.datypeImpl, constraints)
          val modelPath = Paths.get(".", "model", prog.name)
          modelPath.toFile.mkdirs()
          Files.write(modelPath.resolve(s"${ctxt.currentProcedure}_$where.thy"), isabelleTranslation.getBytes())

          val cvc4 = ctxt.exportConstraints(constraints)
          Files.write(modelPath.resolve(s"${ctxt.currentProcedure}_$where.cvc"), cvc4.getBytes())

          val translation = Translation(where, isabelleTranslation, cvc4)

          //        ""
          //      })

          val counterExample: Option[SymbolicCounterExample] = ctxt.check(constraints) match {
            case Unsatisfiable =>
              debugPrint("checkInvariant: unsat, ok")
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
              Some(makeSymbolicCounterExample(
                s"Invariant does not hold $where",
                source.range,
                state,
                Some(model),
                ctxt
              ))
          }
          CheckBooleanExprResult(counterExample, List(translation))
      }
    }

    def checkBooleanExpr(where: String, expr: InExpr, result: Boolean, qVars: Map[InVariable, SymbolicVariable[SymbolicSort]], state: SymbolicState): CheckBooleanExprResult = {
      expr match {
        case TypedAst.QuantifierExpr(_, _, Forall(), vs, body) if result =>
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
                + s"\nInvariant in ${expr.getSource().range}: $expr"
                + s"\nWith variables: " + locals.mkString(", ")
            )
          })
      }
    }


    val results: LazyList[CheckBooleanExprResult] =
      for {
        (inv, i) <- prog.invariants.to(LazyList).zipWithIndex
        if !inv.isFree
      } yield checkBooleanExpr(s"${where}_inv$i", inv.expr, true, Map(), state)

    CheckInvariantResult(results)
  }

  private def invariant(where: String, state: SymbolicState)(implicit ctxt: SymbolicContext): List[NamedConstraint] = {
    for ((inv, i) <- prog.invariants.zipWithIndex) yield {
      val cond = ExprTranslation.translate(inv.expr)(SymbolicSort.bool, ctxt, state)
      NamedConstraint(s"${where}_invariant_$i", cond)
    }
  }


  private def makeVariablesForParameters(ctxt: SymbolicContext, params: List[InVariable]): List[(ProgramVariable, SVal[SortValue])] = {
    for (p <- params) yield
      ProgramVariable(p.name.name) -> ctxt.makeVariable(p.name + "_init")(ctxt.translateSortVal(p.typ))
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
      </modelText>
      {renderResult.toXml}
    </counterExample>
  }

  override def toString: String = s"SymbolicCounterExampleModel($state)"
}

case class CheckInvariantResult(results: LazyList[CheckBooleanExprResult]) {
  def translations: List[Translation] =
    results.flatMap(_.translations).toList

  def ifCounterExample(f: SymbolicCounterExample => Unit): Unit = {
    results.flatMap(r => r.counterExample).headOption match {
      case Some(c) => f(c)
      case None =>
    }
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