package crdtver.symbolic

/**
  * The state of the system.
  */
case class SymbolicState(
  calls: SVal[SortMap[SortCallId, SortOption[SortCall]]],
  happensBefore: SVal[SortMap[SortCallId, SortSet[SortCallId]]],
  callOrigin: SVal[SortMap[SortCallId, SortOption[SortTxId]]],
  transactionOrigin: SVal[SortMap[SortTxId, SortOption[SortInvocationId]]],
  transactionStatus: SVal[SortMap[SortTxId, SortOption[SortTransactionStatus]]],
  generatedIds: SVal[SortMap[SortUid, SortOption[SortInvocationId]]],
  knownIds: SVal[SortSet[SortUid]],
  invocationOp: SVal[SortMap[SortInvocationId, SortOption[SortInvocationInfo]]],
  invocationRes: SVal[SortMap[SortInvocationId, SortOption[SortValue]]],
  currentInvocation: SVal[SortInvocationId],
  localState: Map[ProgramVariable, SVal[_]],
  visibleCalls: SVal[SortSet[SortCallId]],
  satisfiable: Boolean = true
)

case class ProgramVariable(name: String)