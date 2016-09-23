package crdtver

class Interpreter {



  case class AnyValue(value: Any)

  case class TransactionId(id: Int)

  case class CallId(id: Int)

  case class InvocationId(id: Int)

  case class CallInfo(
    id: CallId,
    operationName: String,
    args: List[AnyValue],
    callClock: SnapshotTime,
    callTransaction: TransactionId,
    origin: InvocationId
  )

  case class SnapshotTime(snapshot: Set[CallId])


  case class TransactionInfo(
    id: TransactionId,
    start: SnapshotTime,
    currentCalls: List[CallInfo],
    finished: Boolean
  )

  case class InvocationInfo(
    id: InvocationId,
    procName: String,
    args: List[AnyValue],
    result: AnyValue
  )

  // System state
  case class State(
    // the set of all calls which happened on the database
    calls: Map[CallId, CallInfo] = Map(),
    maxCallId: Int = 0,
    transactions: Map[TransactionId, TransactionInfo] = Map(),
    maxTransactionId: Int = 0,
    invocations: Map[InvocationId, InvocationInfo] = Map(),
    maxInvocationId: Int = 0

  )


  // actions taken by the interpreter
  sealed trait Action

  case class CallAction(
    invocationId: InvocationId,
    procname: String,
    args: List[AnyValue]
  ) extends Action

  case class StartTransaction(
    newTransactionId: TransactionId,
    pulledTransaction: Set[TransactionId]
  ) extends Action





}
