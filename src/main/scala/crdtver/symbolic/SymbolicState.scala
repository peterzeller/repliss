package crdtver.symbolic

/**
  * The state of the system.
  */
case class SymbolicState(
  calls: SymbolicMap[SortCallId, SortOption[SortCall]],
  happensBefore: SymbolicMap[SortCallId, SortSet[SortCallId]],
  callOrigin: SymbolicMap[SortCallId, SortOption[SortTxId]],
  transactionOrigin: SymbolicMap[SortTxId, SortOption[SortInvocationId]],
  transactionStatus: SymbolicMap[SortTxId, SortOption[SortTransactionStatus]],
  generatedIds: SymbolicMap[SortUid, SortOption[SortInvocationId]],
  knownIds: SVal[SortSet[SortUid]],
  invocationOp: SymbolicMap[SortInvocationId, SortOption[SortInvocationInfo]],
  invocationRes: SymbolicMap[SortInvocationId, SortOption[SortValue]],
  currentInvocation: SVal[SortInvocationId],
  currentTransaction: Option[SVal[SortTxId]] = None,
  localState: Map[ProgramVariable, SVal[_]],
  visibleCalls: SymbolicSet[SortCallId],
  satisfiable: Boolean = true
)

case class ProgramVariable(name: String)