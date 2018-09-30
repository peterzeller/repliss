package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.InProgram
import crdtver.symbolic.SymbolicSort._
import crdtver.symbolic.SVal._

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

    // in the beginning everything is unknown so we use symbolic variables:
    val state = SymbolicState(
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
      localState = makeVariablesForParameters(ctxt, proc.params),
      visibleCalls = SSetEmpty()
    )

    // there are a few restrictions we can assume for the initial state:
    // this follows the begin-invoc rule

    // >> invocationOp S i = None;
    ctxt.addConstraint(SMapGet(state.invocationOp, state.currentInvocation) === SNone[SortInvocationInfo]())

    // >> procedure (prog S) procName args ≜ (initState, impl);
    // >>   S'' = (S'⦇localState := (localState S')(i ↦ initState),
    // this is handled by makeVariablesForParameters


    // >>   uniqueIdsInList args ⊆ knownIds S';
    // >>   state_wellFormed S';
    // >>   ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
    val var_tx = "tx" :: txId

    ctxt.addConstraint(forall(var_tx, state.transactionStatus(var_tx) !== SSome(Uncommitted())))

    // >>   invariant_all S';
    ctxt.addConstraint(invariant(state))
    // >>   invocationOp S' i = None;
    val i = ctxt.addUniqueConstant(i, invocationId)

    // >>   prog S' = prog S;
    // >>   currentProc := (currentProc S')(i ↦ impl),
    // >>   visibleCalls := (visibleCalls S')(i ↦ {}),
    // >>   invocationOp := (invocationOp S')(i ↦ (procName, args)) ⦈);
    // >>   valid = invariant_all S'';  ― ‹  TODO check invariant in C ?  ›
    // >>   ⋀tx. transactionOrigin S'' tx ≠ Some i


    val state2 = state.copy(
      invocationOp = SymbolicMapUpdated(i, SSome(???), state.invocationOp) // TODO think about how to represent invocation info
    )

  }

  private def invariant(state: SymbolicState): SVal[SortBoolean] = ???


  private def makeVariablesForParameters(ctxt: SymbolicContext, params: List[InputAst.InVariable]): Map[ProgramVariable, SVal[SortValue]] = {
    params
      .map(p => ProgramVariable(p.name.name) -> ctxt.makeVariable(p.name + "_init")(ctxt.translateSort(p.typ)))
      .toMap
  }

  def translateSort(ctxt: SymbolicContext, typ: InputAst.InTypeExpr): SymbolicSort = {

  }

}
