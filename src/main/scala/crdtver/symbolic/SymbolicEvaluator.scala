package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.InProgram
import crdtver.symbolic.SVal._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.SymbolicSort._

class SymbolicEvaluator(
  val prog: InProgram
) {


  class SymbolicExecutionError(msg: String) extends RuntimeException(msg)

  def checkProgram(): Unit = {
    for (proc <- prog.procedures) {
      checkProcedure(proc)
    }
  }


  private def checkProcedure(proc: InputAst.InProcedure): Unit = {
    val ctxt = new SymbolicContext()

    val params = makeVariablesForParameters(ctxt, proc.params)
    // in the beginning everything is unknown so we use symbolic variables:
    val state: SymbolicState = SymbolicState(
      calls = SymbolicMapVar(ctxt.makeVariable("calls")),
      happensBefore = SymbolicMapVar(ctxt.makeVariable("happensBefore")),
      callOrigin = SymbolicMapVar(ctxt.makeVariable("callOrigin")),
      transactionOrigin = SymbolicMapVar(ctxt.makeVariable("transactionOrigin")),
      transactionStatus = SymbolicMapVar(ctxt.makeVariable("transactionStatus")),
      generatedIds = SymbolicMapVar(ctxt.makeVariable("generatedIds")),
      knownIds = ctxt.makeVariable("knownIds"),
      invocationOp = SymbolicMapVar(ctxt.makeVariable("invocationOp")),
      invocationRes = SymbolicMapVar(ctxt.makeVariable("invocationRes")),
      currentInvocation = ctxt.makeVariable("currentInvocation"),
      localState = params.toMap,
      visibleCalls = SSetEmpty()
    )

    // there are a few restrictions we can assume for the initial state:
    // this follows the begin-invoc rule

    // >> invocationOp S i = None;
    ctxt.addConstraint(SMapGet(state.invocationOp, state.currentInvocation) === SNone[SortInvocationInfo]())

    // >> procedure (prog S) procName args ≜ (initState, impl);
    // >>   uniqueIdsInList args ⊆ knownIds S';
    // >>   state_wellFormed S';
    // >>   ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
    val var_tx = "tx" :: txId

    ctxt.addConstraint(forall(var_tx, state.transactionStatus(var_tx) !== SSome(Uncommitted())))

    // >>   invariant_all S';
    ctxt.addConstraint(invariant(state)(ctxt))
    // >>   invocationOp S' i = None;
    val i = ctxt.addUniqueConstant("i", invocationId)

    // >>   prog S' = prog S;
    // >>   S'' = (S'⦇localState := (localState S')(i ↦ initState),
    // this is handled by makeVariablesForParameters
    // >>   currentProc := (currentProc S')(i ↦ impl),
    // see proc
    // >>   visibleCalls := (visibleCalls S')(i ↦ {}),
    // see state.visibleCalls
    // >>   invocationOp := (invocationOp S')(i ↦ (procName, args)) ⦈);
    // >>   ⋀tx. transactionOrigin S'' tx ≠ Some i
    // >>   valid = invariant_all S'';  ― ‹  TODO check invariant in C ?  ›
    val invocationInfo: SVal[SortOption[SortInvocationInfo]] = SSome(SInvocationInfo(proc.name.name, params.map(_._2)))

    val state2 = state.copy(
      invocationOp = SymbolicMapUpdated(i, invocationInfo, state.invocationOp)
    )

    // check the invariant in state2:
    checkInvariant(ctxt, state2, s"directly after invocation of ${proc.name}") match {
      case Some(msg) =>
        throw new SymbolicExecutionError(msg)
      case None =>
      // ok
    }

    // continue evaluating the procedure body:
    executeStatement(proc.body, state2, ctxt)


  }


  private def executeStatement(stmt: InputAst.InStatement, state: SymbolicState, ctxt: SymbolicContext): SymbolicState = stmt match {
    case InputAst.BlockStmt(source, stmts) =>
      var s = state
      for (stmt <- stmts) {
        s = executeStatement(stmt, s, ctxt)
        if (!s.satisfiable)
          return s
      }
      s
    case InputAst.Atomic(source, body) =>
      val state2 = executeBeginAtomic(state, ctxt)
      val state3 = executeStatement(body, state2, ctxt)
      executeEndAtomic(state3, ctxt)
    case InputAst.LocalVar(source, variable) =>
      // nothing to do
      state
    case InputAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      val condV: SVal[SortBoolean] = ExprTranslation.translate(cond)(bool, ctxt)
      ctxt.inContext(() => {
        // first assume the condition is true
        ctxt.addConstraint(condV)
        ctxt.check() match {
          case Unsatisfiable =>
          // then-branch cannot be taken
          case Unknown | _: Satisfiable =>
            executeStatement(thenStmt, state, ctxt)
        }
      })
      // next assume the condition is false:
      ctxt.addConstraint(SNot(condV))
      ctxt.check() match {
        case Unsatisfiable =>
          // else-branch cannot be taken
          state.copy(satisfiable = false)
        case Unknown | _: Satisfiable =>
          executeStatement(thenStmt, state, ctxt)
      }
    case InputAst.MatchStmt(source, expr, cases) =>
      // TODO
      ???
    case InputAst.CrdtCall(source, call) =>

      // TODO
      ???
    case InputAst.Assignment(source, varname, expr) =>
      // use a new variable here to avoid duplication of expressions
      val v = ctxt.makeVariable(varname.name)(ctxt.translateSortVal(expr.getTyp))
      ctxt.addConstraint(v === ctxt.translateExpr(expr))
      state.copy(localState = state.localState + (ProgramVariable(varname.name) -> v))
    case InputAst.NewIdStmt(source, varname, typename) =>
      val vname = varname.name
      val newV: SVal[SortUid] = ctxt.makeVariable(vname)
      ctxt.addConstraint(state.generatedIds(newV) === SNone())
      state.copy(
        localState = state.localState + (ProgramVariable(vname) -> newV)
      )
    case InputAst.ReturnStmt(source, expr, assertions) =>
      val returnv: SVal[SortValue] = ctxt.translateExpr(expr)

      state.copy(
        invocationRes = SymbolicMapUpdated(state.currentInvocation, SSome(returnv), state.invocationRes)
        // TODO update knownIds
      )
    case InputAst.AssertStmt(source, expr) =>
      ctxt.inContext(() => {
        ctxt.addConstraint(SNot(ctxt.translateExpr(expr)))
        ctxt.check() match {
          case SymbolicContext.Unsatisfiable =>
          // check ok
          case SymbolicContext.Unknown =>
            throw new SymbolicExecutionError(s"Assertion in line ${source.getLine} might not hold.")
          case s: Satisfiable =>
            throw new SymbolicExecutionError(s"Assertion in line ${source.getLine} failed.")
        }
      })
      state
  }

  def executeBeginAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.currentTransaction match {
      case Some(tx) =>
        throw new SymbolicExecutionError(s"Already in a transaction")
      case None =>
        // create variable for the new transaction
        val tx = ctxt.makeVariable[SortTxId]("tx")
        // transactionStatus S t = None;
        ctxt.addConstraint(SEq(SMapGet(state.transactionStatus, tx), SNone[SortTransactionStatus]()))
        // state_monotonicGrowth i S S'
        val state2 = monotonicGrowth(state, ctxt)
        // ⋀t. transactionOrigin S t ≜ i ⟷ transactionOrigin S' t ≜ i; ― ‹No new transactions are added to current invocId.›
        // invariant_all S';
        // ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
        // newTxns ⊆ dom (transactionStatus S');
        // newCalls = callsInTransaction S' newTxns ↓ happensBefore S'
        // vis' = vis ∪ newCalls
        val vis2 = SSetVar(ctxt.makeVariable[SortSet[SortCallId]]("vis"))
        ctxt.addConstraint(SEq(vis2, state2.visibleCalls))
        // ⋀c. callOrigin S' c ≠ Some t




        state2.copy(
          transactionStatus = state2.transactionStatus.put(tx, SSome(Uncommitted())),
          transactionOrigin = state2.transactionOrigin.put(tx, SSome(state2.currentInvocation)),
          currentTransaction = Some(tx),
          visibleCalls = vis2
        )
    }
  }

  def monotonicGrowth(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    // create new variables for new state
    val state2 = state.copy(
      calls = SymbolicMapVar(ctxt.makeVariable("calls")),
      happensBefore = SymbolicMapVar(ctxt.makeVariable("happensBefore")),
      callOrigin = SymbolicMapVar(ctxt.makeVariable("callOrigin")),
      transactionOrigin = SymbolicMapVar(ctxt.makeVariable("transactionOrigin")),
      transactionStatus = SymbolicMapVar(ctxt.makeVariable("transactionStatus")),
      generatedIds = SymbolicMapVar(ctxt.makeVariable("generatedIds")),
      knownIds = ctxt.makeVariable("knownIds")
    )

    // TODO add constraints between old and new state

    state2
  }

  def executeEndAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    // TODO
    ???
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
  private def checkInvariant(ctxt: SymbolicContext, state2: SymbolicState, where: String): Option[String] = {

    ctxt.inContext(() => {
      ctxt.addConstraint(SNot(invariant(state2)(ctxt)))
      ctxt.check() match {
        case Unsatisfiable =>
          // ok
          None
        case Unknown =>
          Some(s"Could not prove invariant $where")
        case s: Satisfiable =>
          val model = ctxt.getModel(s)
          Some(
            s"""Invariant does not hold $where" +
               |Model = $model
                """.stripMargin)
      }
    })
  }

  private def invariant(state: SymbolicState)(implicit ctxt: SymbolicContext): SVal[SortBoolean] = {
    val invExprs: List[SVal[SortBoolean]] = for (inv <- prog.invariants) yield {
      ExprTranslation.translate(inv.expr)(SymbolicSort.bool, ctxt)
    }
    SVal.and(invExprs)
  }


  private def makeVariablesForParameters(ctxt: SymbolicContext, params: List[InputAst.InVariable]): List[(ProgramVariable, SVal[SortValue])] = {
    for (p <- params) yield
      ProgramVariable(p.name.name) -> ctxt.makeVariable(p.name + "_init")(ctxt.translateSortVal(p.typ))
  }


}
