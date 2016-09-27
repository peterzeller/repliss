package crdtver

import crdtver.InputAst.{AnyType, ApplyBuiltin, AssertStmt, Assignment, Atomic, BF_and, BF_equals, BF_getInfo, BF_getOperation, BF_getOrigin, BF_getResult, BF_greater, BF_greaterEq, BF_happensBefore, BF_implies, BF_inCurrentInvoc, BF_isVisible, BF_less, BF_lessEq, BF_not, BF_notEquals, BF_or, BlockStmt, BoolType, CallIdType, CrdtCall, Exists, Forall, FunctionCall, FunctionType, IdType, IfStmt, InExpr, InProcedure, InStatement, InTypeExpr, InVariable, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, LocalVar, MatchStmt, NewIdStmt, OperationType, QuantifierExpr, ReturnStmt, SimpleType, SomeOperationType, UnknownType, UnresolvedType, VarUse}

import scala.collection.immutable.Nil

class Interpreter {


  case class AnyValue(value: Any)

  case class TransactionId(id: Int)

  case class CallId(id: Int)

  case class InvocationId(id: Int)

  case class DataTypeValue(operationName: String, args: List[AnyValue])

  case class CallInfo(
    id: CallId,
    operation: DataTypeValue,
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
      if (x.snapshot == y.snapshot) {
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
    operation: DataTypeValue,
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
              operation = DataTypeValue(functionName.name, args.map(evalExpr(_, newLocalState(), state))),
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
        // TODO check if this is a query
        val eArgs: List[AnyValue] = args.map(evalExpr(_, localState, state))
        AnyValue(DataTypeValue(functionName.name, eArgs))
      case ApplyBuiltin(source, typ, function, args) =>
        val eArgs = args.map(evalExpr(_, localState, state))
        function match {
          case BF_isVisible() =>
            AnyValue(localState.visibleCalls.contains(eArgs.head.value.asInstanceOf[CallId]))
          case BF_happensBefore() =>
            val callId1 = eArgs(0).value.asInstanceOf[CallId]
            val callId2 = eArgs(1).value.asInstanceOf[CallId]
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
            AnyValue(eArgs(0).value == eArgs(1).value)
          case BF_notEquals() =>
            AnyValue(eArgs(0).value != eArgs(1).value)
          case BF_and() =>
            AnyValue(eArgs(0).value.asInstanceOf[Boolean] && eArgs(1).value.asInstanceOf[Boolean])
          case BF_or() =>
            AnyValue(eArgs(0).value.asInstanceOf[Boolean] || eArgs(1).value.asInstanceOf[Boolean])
          case BF_implies() =>
            AnyValue(!eArgs(0).value.asInstanceOf[Boolean] || eArgs(1).value.asInstanceOf[Boolean])
          case BF_not() =>
            AnyValue(!eArgs(0).value.asInstanceOf[Boolean])
          case BF_getOperation() =>
            val info: CallInfo = state.calls(eArgs(0).value.asInstanceOf[CallId])
            AnyValue(info.operation)
          case BF_getInfo() =>
            val info: InvocationInfo = state.invocations(eArgs(0).value.asInstanceOf[InvocationId])
            AnyValue(info.operation)
          case BF_getResult() =>
            val info: InvocationInfo = state.invocations(eArgs(0).value.asInstanceOf[InvocationId])
            info.result
          case BF_getOrigin() =>
            val info: CallInfo = state.calls(eArgs(0).value.asInstanceOf[CallId])
            AnyValue(info.origin)
          case BF_inCurrentInvoc() =>
            ???
        }

      case QuantifierExpr(source, typ, quantifier, vars, e) => quantifier match {
        case Forall() =>
          enumerate(vars, state).forall(m => evalExpr(e, localState.copy(varValues = localState.varValues ++ m), state).value.asInstanceOf[Boolean])
        case Exists() =>
          enumerate(vars, state).exists(m => evalExpr(e, localState.copy(varValues = localState.varValues ++ m), state).value.asInstanceOf[Boolean])
      }
        ???
    }
  }

  def enumerate(vars: List[InVariable], state: State): Stream[Map[String, AnyValue]] = vars match {
    case Nil =>
      Stream.Empty
    case List(v) =>
      enumerateValues(v.typ, state).map(x => Map(v.name.name -> x))
    case v :: tl =>
      val s = enumerateValues(v.typ, state).map(x => Map(v.name.name -> x))
      val tls = enumerate(tl, state)
      s
  }

  def enumerateValues(t: InTypeExpr, state: State): Stream[AnyValue] = t match {
    case AnyType() =>
      ???
    case UnknownType() =>
      ???
    case BoolType() =>
      AnyValue(true) #:: AnyValue(false) #:: Stream.Empty
    case IntType() =>
      ???
    case CallIdType() =>
    case InvocationIdType() =>
    case InvocationInfoType() =>
    case InvocationResultType() =>
    case SomeOperationType() =>
    case OperationType(name, source) =>
    case FunctionType(argTypes, returnType, source) =>
    case SimpleType(name, source) =>
    case IdType(name, source) =>
    case UnresolvedType(name, source) =>
  }



}
