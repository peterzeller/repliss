package crdtver.symbolic

/**
  * The state of the system.
  */
case class SymbolicState(
  symbolicVariables: Set[SymbolicVariable[_]],
  calls: SVal[SortMap[SortCallId, SortOption[SortCall]]],
  happensBefore: SVal[SortMap[SortCallId, SortSet[SortCallId]]],
  callOrigin: SVal[SortMap[SortCallId, SortOption[SortTxId]]],
  transactionOrigin: SVal[SortMap[SortTxId, SortOption[SortInvocationId]]],
  generatedIds: SVal[SortMap[SortUid, SortOption[SortInvocationId]]],
  knownIds: SVal[SortSet[SortUid]],
  invocationOp: SVal[SortMap[SortInvocationId, SortOption[SortInvocationInfo]]],
  invocationRes: SVal[SortMap[SortInvocationId, SortOption[SortValue]]],
  currentInvocation: SVal[SortInvocationId]
)
