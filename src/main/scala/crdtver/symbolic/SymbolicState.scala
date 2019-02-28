package crdtver.symbolic

import crdtver.language.InputAst.{AstElem, IdType, InTypeExpr, SourceRange, SourceTrace}

/**
  * The state of the system.
  */
case class SymbolicState(
  calls: SymbolicMap[SortCallId, SortCall],
  // call -> set of calls that happened before
  happensBefore: SymbolicMap[SortCallId, SortSet[SortCallId]],
  callOrigin: SymbolicMap[SortCallId, SortOption[SortTxId]],
  transactionOrigin: SymbolicMap[SortTxId, SortOption[SortInvocationId]],
  transactionStatus: SymbolicMap[SortTxId, SortOption[SortTransactionStatus]],
  generatedIds: Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]],
  knownIds: Map[IdType, SVal[SortSet[SortCustomUninterpreted]]],
  invocationCalls: SymbolicMap[SortInvocationId, SortSet[SortCallId]],
  invocationOp: SymbolicMap[SortInvocationId, SortInvocationInfo],
  invocationRes: SymbolicMap[SortInvocationId, SortInvocationRes],
  currentInvocation: SVal[SortInvocationId],
  currentTransaction: Option[SVal[SortTxId]] = None,
  localState: Map[ProgramVariable, SVal[_ <: SymbolicSort]],
  visibleCalls: SymbolicSet[SortCallId],
  // for optimizations:
  // store calls that have been made in the current invocation so that we can easily add distinct constraint
  currentCallIds: List[SVal[SortCallId]] = List(),
  satisfiable: Boolean = true,
  //
  trace: Trace
) {
  def lookupLocal(name: String): SVal[_ <: SymbolicSort] =
    localState.get(ProgramVariable(name)) match {
    case Some(value) =>
      value
    case None =>
      throw new RuntimeException(s"could not find variable $name in state $localState")
  }

  def withLocal(pv: ProgramVariable, v: SVal[_ <: SymbolicSort]): SymbolicState =
    this.copy(
      localState = localState + (pv -> v)
    )

  def withTrace(description: String, source: SourceTrace): SymbolicState = {
    this.copy(
      trace = this.trace + TraceStep(description, source)
    )
  }
  def withTrace(description: String, source: AstElem): SymbolicState = {
    withTrace(description, source.getSource())
  }

}

case class ProgramVariable(name: String)

case class Trace(
  // steps in reverse order
  steps: List[TraceStep] = List()
) {
  def +(step: TraceStep): Trace =
    Trace(step::steps)
}


case class TraceStep(
  description: String,
  source: SourceTrace
)
