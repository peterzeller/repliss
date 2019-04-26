package crdtver.symbolic

import java.util.concurrent.TimeUnit

import crdtver.language.InputAst.BuiltInFunc.BF_and
import crdtver.language.InputAst.Forall
import crdtver.language.TypedAst
import crdtver.language.TypedAst._
import crdtver.symbolic.IsabelleTranslation.createIsabelleDefs
import crdtver.symbolic.SVal._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.SymbolicMapVar.symbolicMapVar
import crdtver.symbolic.SymbolicSort._
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, InvocationInfo, LocalVar, SnapshotTime, TransactionId, TransactionInfo}
import crdtver.testing.{Interpreter, Visualization}
import crdtver.utils.PrettyPrintDoc.Doc
import crdtver.utils.{MapWithDefault, StringBasedIdGenerator}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

case class SymbolicExecutionRes(
  proc: String,
  time: Duration,
  error: Option[SymbolicCounterExample]
)

class SymbolicExecutionException(
  val counterExample: SymbolicCounterExample
) extends RuntimeException(counterExample.message) {
  override def toString: String = s"SymbolicExecutionError:\n$counterExample"
}

class SymbolicEvaluator(
  val prog: InProgram
) {


  def checkProgram(): Stream[SymbolicExecutionRes] = {
    debugPrint("checking program")
    for (proc <- prog.procedures.toStream.take(1)) yield checkProcedure(proc)
  }

  private def idTypes(): List[TypedAst.InTypeDecl] =
    prog.types.filter(_.isIdType)


  private def checkProcedure(proc: TypedAst.InProcedure): SymbolicExecutionRes = {
    val startTime = System.currentTimeMillis()
    try {
      debugPrint(s"checking procedure ${proc.name}")
      val z3Translation = new ToSmtTranslation()
      implicit val ctxt: SymbolicContext = new SymbolicContext(z3Translation, proc.name.name, prog)
      z3Translation.datatypeImpl = ctxt.datypeImpl

      val params = makeVariablesForParameters(ctxt, proc.params)

      val generatedIds: Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] =
        generatedIdsVar(ctxt)
      val knownIds: Map[IdType, SVal[SortSet[SortCustomUninterpreted]]] =
        knownIdsVar(ctxt)


      // in the beginning everything is unknown so we use symbolic variables:
      val state: SymbolicState = SymbolicState(
        calls = symbolicMapVar("calls"),
        //[SortCallId, SortOption[SortCall], Nothing](ctxt.makeVariable("calls"))(default()),
        happensBefore = symbolicMapVar("happensBefore"),
        callOrigin = symbolicMapVar("callOrigin"),
        transactionOrigin = symbolicMapVar("transactionOrigin"),
        transactionStatus = symbolicMapVar("transactionStatus"),
        generatedIds = generatedIds,
        knownIds = knownIds,
        invocationCalls = symbolicMapVar("invocationCalls"),
        invocationOp = symbolicMapVar("invocationOp"),
        invocationRes = symbolicMapVar("invocationRes"),
        currentInvocation = ctxt.makeVariable("currentInvocation"),
        localState = params.toMap,
        visibleCalls = SSetEmpty(),
        trace = Trace()
      )

      var constraints = Set[NamedConstraint]()

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

      constraints += NamedConstraint("no_uncommitted_transactions",
        forall(var_tx, state.transactionStatus(var_tx) !== SSome(SUncommitted())))

      // >>   invariant_all S';
      constraints += NamedConstraint("invariant_pre",
        invariant(state)(ctxt))
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

      // there are no calls in the current invocation:
      constraints += NamedConstraint("no_call_in_new_invocation",
        state.invocationCalls.get(i) === SSetEmpty())

      val args: List[SVal[SortValue]] = params.map(_._2)
      // >>   valid = invariant_all S'';  ― ‹  TODO check invariant in C ?  ›
      val invocationInfo: SVal[SortInvocationInfo] = SInvocationInfo(proc.name.name, args)

      val state2 = state.copy(
        invocationOp = state.invocationOp.put(i, invocationInfo),
        constraints = constraints.toList,
        //SymbolicMapUpdated(i, invocationInfo, state.invocationOp)
      ).withTrace(s"Invocation of ${proc.name}(${args.mkString(", ")})", proc.getSource())

      // check the invariant in state2:
      checkInvariant(proc.getSource(), ctxt, state2, s"directly after invocation of ${proc.name}") match {
        case Some(msg) =>
          throw new SymbolicExecutionException(msg)
        case None =>
        // ok
      }

      // continue evaluating the procedure body:
      executeStatement(proc.body, state2, ctxt, (s, _) => s)
      SymbolicExecutionRes(
        proc.name.name,
        Duration.apply(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS),
        None
      )
    } catch {
      case e: SymbolicExecutionException =>
        SymbolicExecutionRes(
          proc.name.name,
          Duration.apply(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS),
          Some(e.counterExample)
        )
    }


  }


  private def knownIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicVariable[SortSet[SortCustomUninterpreted]]] = {
    idTypes().map(t => {
      val idType = IdType(t.name.name)()
      val sort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> ctxt.makeVariable[SortSet[SortCustomUninterpreted]](s"knownIds_${t.name}")(SortSet(sort))
    })
      .toMap
  }

  private def generatedIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] = {
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
    //    debugPrint(str)
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
          checkInvariant(source, ctxt, state4, s"When committing transaction of atomic block in line ${source.getLine}.") match {
            case Some(msg) =>
              throw new SymbolicExecutionException(msg)
            case None =>
            // ok
          }

          follow(state4, ctxt)
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

        val state2 = state.copy(
          calls = state.calls.put(c, callInfo),
          currentCallIds = newCurrentCallIds,
          callOrigin = state.callOrigin.put(c, SSome(t)),
          visibleCalls = state.visibleCalls + c,
          happensBefore = state.happensBefore.put(c, state.visibleCalls),
          invocationCalls = state.invocationCalls.put(state.currentInvocation, SVal.makeSet(newCurrentCallIds))
        ).withTrace(s"call ${call.functionName.name}(${args.mkString(", ")})", source)
          .withConstraints(newConstraints)

        follow(state2, ctxt)
      case TypedAst.Assignment(source, varname, expr) =>
        debugPrint(s"Executing assignment in line ${source.getLine}")
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
          .withConstraint(s"${vname}_new_id_fresh",
            state.generatedIds(idType)(newV) === SNone(SortInvocationId()))
        follow(state2, ctxt)
      case TypedAst.ReturnStmt(source, expr, assertions) =>
        debugPrint(s"Executing return statement in line ${source.getLine}")
        val returnv: SVal[SortValue] = ctxt.translateExprV(expr)

        val state2 = state.copy(
          invocationRes = state.invocationRes.put(state.currentInvocation, SReturnVal(ctxt.currentProcedure, returnv))
          // TODO update knownIds
        ).withTrace(s"Return ${returnv}", source)
        follow(state2, ctxt)
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

        var newConstraints = Set[NamedConstraint]()

        // transactionStatus S t = None;
        // TODO check difference to Isabelle
        newConstraints += NamedConstraint(s"${tx.name}_fresh",
          SEq(SMapGet(state2.transactionStatus, tx), SNone(SortTransactionStatus())))
        // ⋀t. transactionOrigin S t ≜ i ⟷ transactionOrigin S' t ≜ i; ― ‹No new transactions are added to current invocId.›
        val t = ctxt.makeBoundVariable[SortTxId]("t")
        newConstraints += NamedConstraint("no_new_transactions_added_to_current",
          forall(t, (state.transactionOrigin.get(t) === SSome(state.currentInvocation))
            === (state2.transactionOrigin.get(t) === SSome(state.currentInvocation)))
        )
        // no new calls are added to current invocation:
        newConstraints += NamedConstraint("no_new_calls_addded_to_current",
          SEq(
            state.invocationCalls.get(state.currentInvocation),
            state2.invocationCalls.get(state.currentInvocation)))
        // invariant_all S';
        newConstraints += NamedConstraint("invariant_before_transaction",
          invariant(state2)(ctxt))
        // ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
        val tx2 = ctxt.makeBoundVariable[SortTxId]("tx2")
        newConstraints += NamedConstraint("no_uncommitted_transactions",
          forall(tx2, state2.transactionStatus.get(tx2) !== SSome(SUncommitted()))
        )
        // newTxns ⊆ dom (transactionStatus S');
        val newTxns = SSetVar[SortTxId](ctxt.makeVariable("newTxns"))
        newConstraints += NamedConstraint("new_transactions_exist",
          newTxns.isSubsetOf(MapDomain(state2.transactionStatus))
        )


        // newCalls = callsInTransaction S' newTxns ↓ happensBefore S'
        val newCalls = SSetVar[SortCallId](ctxt.makeVariable("newCalls"))
        // TODO add restrictions to newCalls
        // vis' = vis ∪ newCalls
        val vis2 = SSetVar(ctxt.makeVariable[SortSet[SortCallId]]("vis"))
        newConstraints += NamedConstraint("vis_update",
          SEq(vis2, SSetUnion(state2.visibleCalls, newCalls)))
        // TODO ⋀c. callOrigin S' c ≠ Some t


        state2.copy(
          transactionStatus = state2.transactionStatus.put(tx, SSome(SUncommitted())),
          transactionOrigin = state2.transactionOrigin.put(tx, SSome(state2.currentInvocation)),
          currentTransaction = Some(tx),
          visibleCalls = vis2
        ).withTrace("Start of transaction", source)
          .withConstraints(newConstraints)
    }
  }

  /** *
    * Adds assumptions that the given state is well-formed.
    */
  def assumeWellformed(where: String, state: SymbolicState, ctxt: SymbolicContext): Iterable[NamedConstraint] = {
    val constraints = mutable.ListBuffer[NamedConstraint]()

    {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")
      // definition of invocationCalls
      constraints += NamedConstraint(s"${where}_WF_invocationCalls",
        forall(i, forall(c, SEq(
          SSetContains(state.invocationCalls.get(i), c),
          exists(tx,
            SAnd(
              SEq(state.callOrigin.get(c), SSome(tx)),
              SEq(state.transactionOrigin.get(tx), SSome(i))))))))
    }
    {
      val c = ctxt.makeBoundVariable[SortCallId]("c")


      // domain calls = domain callsOrigin
      constraints += NamedConstraint(s"${where}_WF_callOrigin",
        forall(c,
          SEq(
            SEq(state.callOrigin.get(c), SNone(SortTxId())),
            SEq(state.calls.get(c), SCallInfoNone()))))
    }


    {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      // when the transaction status is none, then there can be no calls in the transaction
      constraints += NamedConstraint(s"${where}_WF_transactionStatus_callOrigin",
        forall(tx,
          SImplies(
            SEq(state.transactionStatus.get(tx), SNone(SortTransactionStatus())),
            forall(c, SNotEq(state.callOrigin.get(c), SSome(tx))))))
    }

    {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      constraints += NamedConstraint(s"${where}_WF_no_call_implies_no_happensBefore",
        forall(c,
          SImplies(
            SEq(state.callOrigin.get(c), SNone(SortTxId())),
            SEq(state.happensBefore.get(c), SSetEmpty[SortCallId]()))))
    }

    {
      val ca = ctxt.makeBoundVariable[SortCallId]("ca")
      val cb = ctxt.makeBoundVariable[SortCallId]("cb")
      constraints += NamedConstraint(s"${where}_WF_no_call_implies_not_in_happensBefore",
        forall(ca, forall(cb,
          SImplies(
            SEq(state.callOrigin.get(ca), SNone(SortTxId())),
            SNot(SSetContains(state.happensBefore.get(cb), ca))))))
    }

    { // callOrigin exists
      val ca = ctxt.makeBoundVariable[SortCallId]("ca")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")
      constraints += NamedConstraint(s"${where}_WF_callOrigin_exists",
        forall(ca, forall(tx,
          SImplies(
            SEq(state.callOrigin.get(ca), SSome(tx)),
            SNotEq[SortOption[SortInvocationId]](state.transactionOrigin.get(tx), SNone(SortInvocationId()))))))
    }

    { // transactionOrigin exists
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")
      constraints += NamedConstraint(s"${where}_WF_transactionOrigin_exists",
        forall(tx, forall(i,
          SImplies(
            SEq(state.transactionOrigin.get(tx), SSome(i)),
            SNotEq[SortInvocationInfo](state.invocationOp.get(i), SInvocationInfoNone())))))
    }

    {
      // all parameters of method invocations are known ids
      for (proc <- prog.procedures) {
        for ((arg, argI) <- proc.params.zipWithIndex) {
          arg.typ match {
            case t: IdType =>
              val i = ctxt.makeBoundVariable[SortInvocationId]("i")
              val argVariables: List[SymbolicVariable[SortValue]] = proc.params.map(p => ctxt.makeBoundVariable[SortValue](p.name.name)(ExprTranslation.translateType(p.typ)(ctxt).asInstanceOf[SortValue]))
              val knownIds: SymbolicVariable[SortSet[SortCustomUninterpreted]] = knownIdsVar(ctxt)(t)
              constraints += NamedConstraint(s"${where}_${proc.name.name}_parameter_${arg.name}_known",
                forallL(i :: argVariables,
                  SImplies(
                    SEq(state.invocationOp.get(i), SInvocationInfo(proc.name.name, argVariables)),
                    SSetContains(knownIds, argVariables(argI).asInstanceOf[SVal[SortCustomUninterpreted]])
                  ))
              )
            case _ =>
            // should also handle nested ids
          }
        }
      }
    }

    {
      // all returned values of method invocations are known ids
      for (proc <- prog.procedures) {
        proc.returnType match {
          case Some(returnType) =>
            returnType match {

              case t: IdType =>
                val i = ctxt.makeBoundVariable[SortInvocationId]("i")
                val r = ctxt.makeBoundVariable("result")(ExprTranslation.translateType(t)(ctxt).asInstanceOf[SortCustomUninterpreted])
                val knownIds: SymbolicVariable[SortSet[SortCustomUninterpreted]] = knownIdsVar(ctxt)(t)
                constraints += NamedConstraint(s"${where}_${proc.name.name}_result_known",
                  forallL(List(i, r),
                    SImplies(
                      SEq(state.invocationRes.get(i), SReturnVal(proc.name.name, r.upcast)),
                      SSetContains(knownIds, r)
                    ))
                )
              case _ =>
              // should also handle nested ids
            }
          case None =>
        }
      }


    }

    {
      // if an id is known it was generated
      for ((t, knownIds) <- state.knownIds) {
        val x = ctxt.makeBoundVariable[SortCustomUninterpreted]("x")(SortCustomUninterpreted(t.name))
        constraints += NamedConstraint(s"${where}_${t.name}_knownIds_are_generated",
          forall(x,
            SImplies(
              SSetContains(knownIds, x),
              SNotEq(state.generatedIds(t).get(x), SNone(SortInvocationId())))
          )
        )
      }
    }

    // TODO compare with WhyTranslation.wellformedConditions

    constraints.toList
  }

  def monotonicGrowth(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    implicit val ictxt: SymbolicContext = ctxt
    // create new variables for new state
    val state2 = state.copy(
      calls = symbolicMapVar("calls"),
      happensBefore = symbolicMapVar("happensBefore"),
      callOrigin = symbolicMapVar("callOrigin"),
      transactionOrigin = symbolicMapVar("transactionOrigin"),
      transactionStatus = symbolicMapVar("transactionStatus"),
      invocationCalls = symbolicMapVar("invocationCalls"),
      generatedIds = generatedIdsVar,
      knownIds = knownIdsVar
    )
    var constraints = Set[NamedConstraint]()

    {
      val c = ctxt.makeVariable[SortCallId]("c")
      val tx = ctxt.makeVariable[SortTxId]("tx")
      constraints += NamedConstraint("growth_callOrigin",
        forall(c, forall(tx,
          SImplies(
            SEq(state.callOrigin.get(c), SSome(tx)),
            SEq(state2.callOrigin.get(c), SSome(tx))))))
    }

    {
      val tx = ctxt.makeVariable[SortTxId]("tx")
      constraints += NamedConstraint("growth_transactionStatus",
        forall(tx, SImplies(
          SEq(state.transactionStatus.get(tx), SSome(SCommitted())),
          SEq(state2.transactionStatus.get(tx), SSome(SCommitted())))))
    }

    // TODO add more constraints between old and new state
    constraints ++= assumeWellformed("transaction_begin", state2, ctxt)

    state2.withConstraints(constraints)
  }

  def executeEndAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.copy(
      currentTransaction = None,
      transactionStatus = state.transactionStatus.put(state.currentTransaction.get, SSome(SCommitted()))
    )
  }


  def printModel(model: Model, state: SymbolicState): Doc = {
    import crdtver.utils.PrettyPrintDoc._

    implicit def exprToDoc(e: SVal[_]): Doc = e.toString

    implicit def mapToDoc[K, V](m: Map[K, V]): Doc =
      nested(2, line <> sep(line, m.map(e => e._1.toString <+> "->" <+> e._2.toString).toList))

    implicit def setToDoc[T](m: Set[T]): Doc =
      "{" <> sep(", ", m.map("" <> _.toString).toList) <> "}"

    state.trace.toString </>
      "calls = " <> extractMap(model.evaluate(state.calls)) </>
      "happensBefore = " <> extractMap(model.evaluate(state.happensBefore)).mapValues(extractSet) </>
      "callOrigin = " <> extractMap(model.evaluate(state.callOrigin)) </>
      "transactionOrigin = " <> extractMap(model.evaluate(state.transactionOrigin)) </>
      "transactionStatus = " <> extractMap(model.evaluate(state.transactionStatus)) </>
      //      "generatedIds = " <> model.evaluate(state.generatedIds) </>
      //      "knownIds = " <> model.evaluate(state.knownIds) </>
      "invocationCalls = " <> extractMap(model.evaluate(state.invocationCalls)).mapValues(extractSet) </>
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

    val isabelleTranslation: String = createIsabelleDefs(name, ctxt.datypeImpl, state.constraints)
    val smtTranslation = ctxt.exportConstraints(state.constraints)

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
      isabelleTranslation = isabelleTranslation,
      smtTranslation = smtTranslation
    )
  }

  private def extractModel(state: SymbolicState, model: Model): SymbolicCounterExampleModel = {

    val iState: Interpreter.State = extractInterpreterState(state, model)

    debugPrint(s"STATE =\n$iState")

    val modelText: Doc = printModel(model, state)

    val (dot, svg) = Visualization.renderStateGraph(prog, iState)
    SymbolicCounterExampleModel(
      iState,
      modelText,
      svg,
      dot
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
  private def checkInvariant(source: SourceTrace, ctxt: SymbolicContext, state: SymbolicState, where: String): Option[SymbolicCounterExample] = {

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
    def checkSVal(expr: SVal[SortBoolean], result: Boolean, state1: SymbolicState): Option[SymbolicCounterExample] = {
      val constraint =
        if (result) {
          NamedConstraint("invariant_not_violated", SNot(expr))
        } else {
          NamedConstraint("invariant_not_violated", expr)
        }
      val state = state1.withConstraints(List(constraint))
//      createIsabelleDefs(s"${
//        ctxt.currentProcedure
//      }_$where", ctxt.datypeImpl, state.constraints)
      ctxt.check(state.constraints) match {
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
    }

    def checkBooleanExpr(expr: InExpr, result: Boolean, qVars: Map[InVariable, SymbolicVariable[SymbolicSort]], state: SymbolicState): Option[SymbolicCounterExample] = {
      expr match {
        case TypedAst.QuantifierExpr(_, _, Forall(), vs, body) if result =>
          val vars: Map[InVariable, SymbolicVariable[SymbolicSort]] = vs.map(v => v -> ctxt.makeVariable(v.name.name)(ExprTranslation.translateType(v.typ)(ctxt))).toMap

          val state2 = vars.foldLeft(state)((s, p) => s.withLocal(ProgramVariable(p._1.name.name), p._2))

          checkBooleanExpr(body, result, qVars ++ vars, state2)
        case ApplyBuiltin(_, _, BF_and(), List(l, r)) =>
          if (result) {
            checkBooleanExpr(l, result, qVars, state).orElse(checkBooleanExpr(l, result, qVars, state))
          } else {
            ???
          }
        case _ =>
          checkSVal(ExprTranslation.translate(expr)(SymbolicSort.bool, ctxt, state), result, state).map((ce: SymbolicCounterExample) => {


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


    val results: Stream[SymbolicCounterExample] =
      for {
        inv <- prog.invariants.toStream
        ce <- checkBooleanExpr(inv.expr, true, Map(), state)
      } yield ce

    results.headOption
  }

  private def invariant(state: SymbolicState)(implicit ctxt: SymbolicContext): SVal[SortBoolean] = {
    val invExprs: List[SVal[SortBoolean]] = for (inv <- prog.invariants) yield {
      ExprTranslation.translate(inv.expr)(SymbolicSort.bool, ctxt, state)
    }
    SVal.and(invExprs)
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
  isabelleTranslation: String,
  smtTranslation: String
)

case class SymbolicCounterExampleModel(
  state: Interpreter.State,
  modelText: Doc,
  counterExampleSvg: String,
  counterExampleDot: String
) {
  override def toString: String = s"SymbolicCounterExampleModel($state)"
}