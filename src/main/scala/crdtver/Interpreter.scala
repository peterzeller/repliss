package crdtver

import crdtver.InputAst.{ApplyBuiltin, AssertStmt, Assignment, Atomic, BF_and, BF_equals, BF_getInfo, BF_getOperation, BF_getOrigin, BF_getResult, BF_greater, BF_greaterEq, BF_happensBefore, BF_implies, BF_inCurrentInvoc, BF_isVisible, BF_less, BF_lessEq, BF_not, BF_notEquals, BF_or, BlockStmt, CrdtCall, FunctionCall, IfStmt, InExpr, InProcedure, InStatement, LocalVar, MatchStmt, NewIdStmt, QuantifierExpr, ReturnStmt, VarUse}

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

  case class SnapshotTime(snapshot: Set[CallId]) {
    def includes(call: CallInfo): Boolean = {
      snapshot.contains(call.id)
    }
  }

  implicit object SnapshotTimeOrder extends PartialOrdering[SnapshotTime] {
    override def tryCompare(x: SnapshotTime, y: SnapshotTime): Option[Int] = {
      if (x.asInstanceOf == y.snapshot) {
        Some(0)
      } else if (x.snapshot.subsetOf(y.snapshot)) {
        Some(-1)
      } else if (y.snapshot.subsetOf(x.snapshot)) {
        Some(1)
      } else {
        None
      }
    }

    override def lteq(x: SnapshotTime, y: SnapshotTime): Boolean = {
      x.snapshot.subsetOf(y.snapshot)
    }
  }


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


  // local state for one invocation
  case class LocalState(
    varValues: Map[String, AnyValue],
    todo: List[StatementOrAction],
    waitingFor: LocalWaitingFor,
    currentTransaction: Option[TransactionInfo],
    visibleCalls: Set[CallId]
  )

  sealed abstract class LocalWaitingFor

  case class BeginTransaction() extends LocalWaitingFor

  case class FinishInvocation() extends LocalWaitingFor


  sealed abstract class StatementOrAction

  case class ExecStmt(inStatement: InStatement) extends StatementOrAction

  case class EndAtomic() extends StatementOrAction


  // System state
  case class State(
    // the set of all calls which happened on the database
    calls: Map[CallId, CallInfo] = Map(),
    maxCallId: Int = 0,
    transactions: Map[TransactionId, TransactionInfo] = Map(),
    maxTransactionId: Int = 0,
    invocations: Map[InvocationId, InvocationInfo] = Map(),
    maxInvocationId: Int = 0,
    localStates: Map[InvocationId, LocalState]

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


  /**
    * this is like a big-step semantics for local executions
    */
  def executeLocal(invocationId: InvocationId, inState: State): State = {
    var state = inState
    val localState = inState.localStates(invocationId)
    var todo = localState.todo
    var varValues = localState.varValues
    var waitingFor: LocalWaitingFor = FinishInvocation()
    var currentTransaction = localState.currentTransaction
    var visibleCalls = localState.visibleCalls
    while (todo.nonEmpty) {
      val action = todo.head
      todo = todo.tail
      action match {
        case EndAtomic() =>
        case ExecStmt(s) => s match {
          case BlockStmt(source, stmts) =>
            todo = stmts.map(ExecStmt) ++ todo
          case Atomic(source, body) =>
            waitingFor = BeginTransaction()
            todo = ExecStmt(body) +: EndAtomic() +: todo
          case LocalVar(source, variable) =>
          case IfStmt(source, cond, thenStmt, elseStmt) =>
            if (evalExpr(cond, newLocalState(), state).value == true) {
              todo = ExecStmt(thenStmt) +: todo
            } else {
              todo = ExecStmt(elseStmt) +: todo
            }
          case MatchStmt(source, expr, cases) =>
            ???
          case CrdtCall(source, FunctionCall(_, _, functionName, args)) =>
            val newCallId = CallId(state.maxCallId + 1)
            state = state.copy(maxCallId = newCallId.id)

            val newCallInfo: CallInfo = CallInfo(
              id = newCallId,
              operationName = functionName.name,
              args = args.map(evalExpr(_, newLocalState(), state)),
              callClock = ???,
              callTransaction = ???,
              origin = ???
            )
            currentTransaction = currentTransaction.map(tr => tr.copy(
              currentCalls = tr.currentCalls :+ newCallInfo
            ))
            visibleCalls = visibleCalls + newCallId
          case Assignment(source, varname, expr) =>
          case NewIdStmt(source, varname, typename) =>
          case ReturnStmt(source, expr, assertions) =>
          case AssertStmt(source, expr) =>
        }
      }
    }
    def newLocalState(): LocalState = LocalState(
      varValues = varValues,
      todo = todo,
      waitingFor = waitingFor,
      currentTransaction = currentTransaction,
      visibleCalls = visibleCalls
    )
    state.copy(
      localStates = state.localStates + (invocationId -> newLocalState)
    )
  }

  def applyTransaction(currentTransaction: Option[TransactionInfo], inState: State) = inState

  def evalExpr(expr: InExpr, localState: LocalState, inState: State): AnyValue = {
    val state = applyTransaction(localState.currentTransaction, inState)

    expr match {
      case VarUse(source, typ, name) =>
        localState.varValues(name)
      case FunctionCall(source, typ, functionName, args) =>
        ???
      case ApplyBuiltin(source, typ, function, args) =>
        val eArgs = args.map(evalExpr(_, localState, state))
        function match {
          case BF_isVisible() =>
            AnyValue(localState.visibleCalls.contains(eArgs.head.value.asInstanceOf[CallId]))
          case BF_happensBefore() =>
            val callId1 = eArgs.head.value.asInstanceOf[CallId]
            val callId2 = eArgs.head.value.asInstanceOf[CallId]
            val call1 = state.calls(callId1)
            val call2 = state.calls(callId2)
            AnyValue(
                 localState.visibleCalls.contains(callId1)
              && localState.visibleCalls.contains(callId2)
              && call2.callClock.includes(call1)
            )
          case BF_less() =>
            ???

          case BF_lessEq() =>
            ???
          case BF_greater() =>
            ???
          case BF_greaterEq() =>
            ???
          case BF_equals() =>
            ???
          case BF_notEquals() =>
            ???
          case BF_and() =>
            ???
          case BF_or() =>
            ???

          case BF_implies() =>
            ???
          case BF_not() =>
            ???
          case BF_getOperation() =>
            ???
          case BF_getInfo() =>
            ???
          case BF_getResult() =>
            ???
          case BF_getOrigin() =>
            ???
          case BF_inCurrentInvoc() =>
            ???
        }

      case QuantifierExpr(source, typ, quantifier, vars, expr) =>
    }
  }


}
