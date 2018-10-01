package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.InProgram
import crdtver.symbolic.SVal._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.SymbolicSort._

class SymbolicEvaluator(
  val prog: InProgram
) {


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
      calls = ctxt.makeVariable("calls"),
      happensBefore = ctxt.makeVariable("happensBefore"),
      callOrigin = ctxt.makeVariable("callOrigin"),
      transactionOrigin = ctxt.makeVariable("transactionOrigin"),
      transactionStatus = ctxt.makeVariable("transactionStatus"),
      generatedIds = ctxt.makeVariable("generatedIds"),
      knownIds = ctxt.makeVariable("knownIds"),
      invocationOp = ctxt.makeVariable("invocationOp"),
      invocationRes = ctxt.makeVariable("invocationRes"),
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
        throw new RuntimeException(msg)
      case None =>
      // ok
    }

    // continue evaluating the procedure body:
    executeStatement(proc.body, state2, ctxt)


  }


  private def executeStatement(stmt: InputAst.InStatement, state: SymbolicState, ctxt: SymbolicContext): SymbolicState = stmt match {
    case InputAst.BlockStmt(source, stmts) =>
      stmts.foldLeft(state)((state, stmt) => executeStatement(stmt, state, ctxt))
    case InputAst.Atomic(source, body) =>
      val state2 = executeBeginAtomic(state, ctxt)
      val state3 = executeStatement(body, state2, ctxt)
      executeEndAtomic(state3, ctxt)
    case InputAst.LocalVar(source, variable) =>
      // TODO
      ???
    case InputAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      // TODO
      ???
    case InputAst.MatchStmt(source, expr, cases) =>
      // TODO
      ???
    case InputAst.CrdtCall(source, call) =>
      // TODO
      ???
    case InputAst.Assignment(source, varname, expr) =>
      // TODO
      ???
    case InputAst.NewIdStmt(source, varname, typename) =>
      // TODO
      ???
    case InputAst.ReturnStmt(source, expr, assertions) =>
      // TODO
      ???
    case InputAst.AssertStmt(source, expr) =>
      // TODO
      ???
  }

  def executeBeginAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    // TODO
    ???
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
        case Unsatisfiable() =>
          // ok
          None()
        case Unknown() =>
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
