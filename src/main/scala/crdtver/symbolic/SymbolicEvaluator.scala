package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.InProgram

class SymbolicEvaluator(
  val prog: InProgram
) {


  def checkProgram(): Unit = {
    for (proc <- prog.procedures) {
      checkProcedure(proc)
    }
  }


  private def checkProcedure(proc: InputAst.InProcedure): Unit = {
    // first we need to create an additional symbolic state
    // this follows the begin-invoc rule

    val ctxt = new SymbolicContext()



    // TODO create variables for all the fields
    val state = SymbolicState(
      symbolicVariables = Set(),
      calls = ctxt.makeVariable("calls"),
      happensBefore = ???,
      callOrigin = ???,
      transactionOrigin = ???,
      generatedIds = ???,
      knownIds = ???,
      invocationOp = ???,
      invocationRes = ???,
      currentInvocation = ???
    )



    //invocationOp S i = None;
    ctxt.addConstraint(SMapGet(state.invocationOp, state.currentInvocation) === SNone[SortInvocationInfo]())


  }

}
