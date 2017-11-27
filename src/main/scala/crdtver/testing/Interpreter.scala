package crdtver.testing

import crdtver.language.{InputAst}
import crdtver.language.InputAst._

import scala.collection.immutable.{::, Nil}


class Interpreter(prog: InProgram, domainSize: Int = 3) {

  import Interpreter._

  val debug = true

  def debugLog(s: String): Unit = {
    if (debug) {
      println(s)
    }
  }


  def happensBefore(state: State, c1: CallId, c2: CallId): Boolean = {
    val ci1 = state.calls(c1)
    val ci2 = state.calls(c2)
    ci1.callClock.happensBefore(ci2.callClock)
  }

  def happensAfter(state: State, c1: CallId, c2: CallId): Boolean = {
    val ci1 = state.calls(c1)
    val ci2 = state.calls(c2)
    ci1.callClock.happensAfter(ci2.callClock)
  }

  def calculatePulledCalls(state: State, visibleCalls: Set[CallId], pulledTransactions: Set[TransactionId]): Set[CallId] = {
    var pulledCalls = Set[CallId]()
    val pulledTransactions2 = pulledTransactions.filter { tr =>
      state.transactions.get(tr) match {
        case Some(trInfo) => trInfo.finished
        case None => false
      }
    }

    // get pulled calls
    for ((c, i) <- state.calls) {
      if (pulledTransactions2.contains(i.callTransaction)) {
        pulledCalls += c
      }
    }
    var calls = visibleCalls
    // get causally dependent calls
    for (c <- pulledCalls) {
      val ci = state.calls(c)
      calls += c
      calls ++= ci.callClock.snapshot
    }
    //    for (c1 <- state.calls.keySet) {
    //      for (c2 <- pulledCalls) {
    //        if (happensBefore(state, c1, c2)) {
    //          calls += c1
    //        }
    //      }
    //    }
    calls
  }

  def extractIdsList(args: List[AnyValue], argTypes: List[InTypeExpr]): Map[IdType, Set[AnyValue]] = {
    val idList = args.zip(argTypes).map(a => extractIds(a._1, Some(a._2)))
    idList.fold(Map())(_ ++ _)
  }

  def extractIds(result: AnyValue, returnType: Option[InTypeExpr]): Map[IdType, Set[AnyValue]] = returnType match {
    case Some(t) =>
      t match {
        case idt@IdType(name, source) =>
          Map(idt -> Set(result))
        case _ =>
          // TODO handle datatypes with nested ids
          Map()
      }
    case None =>
      Map()
  }


  def mergeMultimap[K, V](a: Map[K, Set[V]], b: Map[K, Set[V]]): Map[K, Set[V]] = {
    (for (k <- a.keySet ++ b.keySet) yield k -> (a.getOrElse(k, Set()) ++ b.getOrElse(k, Set()))).toMap
  }

  def executeAction(state: State, action: Action): Option[State] = {
    //    val invocInfo = state.invocations.get(action.invocationId) match {
    //      case Some(invocInfo) => invocInfo.operation.operationName
    //      case None => "new"
    //    }
    //    debugLog(s"execute action $invocInfo $action")
    action match {
      case InvariantCheck(invocationId) =>
        state.localStates.get(invocationId) match {
          case Some(ls) =>
            checkInvariantsLocal(state, ls)
            None
          case None => None
        }
      case CallAction(invocationId, procname, args) =>
        if (state.invocations.contains(invocationId)) {
          // already has invocation with this key
          return None
        }


        val proc: InProcedure = prog.findProcedure(procname)
        val varvalues = (for ((param, arg) <- proc.params.zip(args)) yield {
          LocalVar(param.name.name) -> arg
        }).toMap

        val argIds = extractIdsList(args, proc.params.map(_.typ))
        for ((t, ids) <- argIds) {
          val known = state.knownIds.getOrElse(t, Set())
          if (ids.exists(id => !known.contains(id))) {
            // Id is not known yet
            return None
          }
        }

        val newState = state.copy(
          invocations = state.invocations
            + (invocationId -> InvocationInfo(
            id = invocationId,
            operation = DataTypeValue(procname, args),
            result = None
          )),
          maxInvocationId = state.maxInvocationId.max(invocationId.id),
          localStates = state.localStates
            + (invocationId -> LocalState(
            varValues = varvalues,
            todo = List(ExecStmt(proc.body)),
            waitingFor = WaitForBegin(),
            currentTransaction = None,
            visibleCalls = Set()
          ))
        )
        Some(executeLocal(invocationId, newState))
      case LocalAction(invocationId, localAction) =>
        val localStateOpt = state.localStates.get(invocationId)
        if (localStateOpt.isEmpty) {
          return None
        }
        val localState = localStateOpt.get
        val waitingFor = localState.waitingFor
        val newState: State = localAction match {
          case StartTransaction(newTransactionId, pulledTransaction) =>
            waitingFor match {
              case WaitForBeginTransaction() =>
                val newVisibleCalls = calculatePulledCalls(state, localState.visibleCalls, pulledTransaction)
                //                debugLog(s"Starting transaction $newTransactionId")
                //                debugLog(s"   old visible = ${localState.visibleCalls}")
                //                debugLog(s"   new visible = $newVisibleCalls")

                val newTransactionInfo = TransactionInfo(
                  id = newTransactionId,
                  origin = invocationId,
                  start = SnapshotTime(newVisibleCalls),
                  currentCalls = List(),
                  finished = false
                )
                val newLocalState: LocalState = localState.copy(
                  visibleCalls = newVisibleCalls,
                  currentTransaction = Some(newTransactionInfo)
                )
                state.copy(
                  localStates = state.localStates + (invocationId -> newLocalState),
                  maxTransactionId = state.maxTransactionId.max(newTransactionId.id)
                )
              case _ =>
                return None
            }
          case Fail() =>
            //            val newInvocationInfo = state.invocations(invocationId).copy(
            //              result = ???
            //            )
            // remove local state
            state.copy(
              localStates = state.localStates - invocationId
              //              invocations = state.invocations + (invocationId -> newInvocationInfo)

            )
          case Return() =>
            waitingFor match {
              case WaitForFinishInvocation(result: AnyValue) =>
                val invocationInfo = state.invocations(action.invocationId)
                val resultDt = DataTypeValue(s"${invocationInfo.operation.operationName}_res", List(result))
                val newInvocationInfo = state.invocations(invocationId).copy(
                  result = Some(AnyValue(resultDt))
                )

                val returnType = prog.findProcedure(newInvocationInfo.operation.operationName).returnType

                val newKnownIds: Map[IdType, Set[AnyValue]] = extractIds(result, returnType)

                // finish current transaction if any
                val state2 = localState.currentTransaction match {
                  case Some(currentTr) =>
                    val tr = currentTr.copy(
                      finished = true
                    )
                    state.copy(
                      calls = state.calls ++ tr.currentCalls.map(c => c.id -> c).toMap,
                      transactions = state.transactions + (tr.id -> tr)
                    )
                  case None =>
                    state
                }


                // remove local state
                return Some(state2.copy(
                  localStates = state.localStates - invocationId,
                  invocations = state.invocations + (invocationId -> newInvocationInfo),
                  knownIds = mergeMultimap(state.knownIds, newKnownIds)
                ))
              case _ =>
                return None
            }
          case NewId(id) =>
            waitingFor match {
              case WaitForNewId(varname, typename) =>
                val newLocalState = localState.copy(
                  varValues = localState.varValues + (LocalVar(varname) -> AnyValue(s"${typename}_${"%03d".format(id)}"))
                )
                state.copy(
                  localStates = state.localStates + (invocationId -> newLocalState)
                )
              case _ =>
                return None
            }
        }
        Some(executeLocal(invocationId, newState))
    }

  }

  /**
    * this is like a big-step semantics for local executions
    */
  def executeLocal(invocationId: InvocationId, inState: State): State = {
    var state = inState
    val localState = inState.localStates(invocationId)
    var todo = localState.todo
    var varValues = localState.varValues
    var waitingFor: Option[LocalWaitingFor] = None
    var currentTransaction = localState.currentTransaction
    var visibleCalls = localState.visibleCalls
    while (todo.nonEmpty) {
      val action = todo.head
      todo = todo.tail
      action match {
        case EndAtomic() =>
          var tr = currentTransaction.get
          tr = tr.copy(
            finished = true
          )
          state = state.copy(
            calls = state.calls ++ tr.currentCalls.map(c => c.id -> c).toMap,
            transactions = state.transactions + (tr.id -> tr)
          )
          currentTransaction = None
        case ExecStmt(s) => s match {
          case BlockStmt(source, stmts) =>
            todo = stmts.map(ExecStmt) ++ todo
          case Atomic(source, body) =>
            waitingFor = Some(WaitForBeginTransaction())
            todo = ExecStmt(body) +: EndAtomic() +: todo
            return yieldState()
          case InputAst.LocalVar(source, variable) =>
          case IfStmt(source, cond, thenStmt, elseStmt) =>
            val condVal = evalExpr(cond, newLocalState(), state)(defaultAnyValueCreator).value
            if (condVal.asInstanceOf[Boolean]) {
              todo = ExecStmt(thenStmt) +: todo
            } else {
              todo = ExecStmt(elseStmt) +: todo
            }
          case MatchStmt(source, expr, cases) =>
            ???
          case CrdtCall(source, FunctionCall(_, _, functionName, args)) =>
            val newCallId = CallId(state.maxCallId + 1)
            state = state.copy(maxCallId = newCallId.id)

            visibleCalls = visibleCalls + newCallId
            val newCallInfo: CallInfo = CallInfo(
              id = newCallId,
              operation = DataTypeValue(functionName.name, args.map(evalExpr(_, newLocalState(), state)(defaultAnyValueCreator))),
              callClock = SnapshotTime(visibleCalls),
              callTransaction = currentTransaction.get.id,
              origin = invocationId
            )
            currentTransaction = currentTransaction.map(tr => tr.copy(
              currentCalls = tr.currentCalls :+ newCallInfo
            ))

          case Assignment(source, varname, expr) =>
            val e = evalExpr(expr, newLocalState(), state)(defaultAnyValueCreator)
            varValues = varValues + (LocalVar(varname.name) -> e)
          case NewIdStmt(source, varname, typename) =>
            waitingFor = Some(WaitForNewId(varname.name, typename))
            return yieldState()
          case ReturnStmt(source, expr, assertions) =>
            waitingFor = Some(WaitForFinishInvocation(evalExpr(expr, newLocalState(), state)(defaultAnyValueCreator)))
            return yieldState()
          case AssertStmt(source, expr) =>
            ???
        }
      }
    }

    // local helper functions
    def newLocalState(): LocalState = LocalState(
      varValues = varValues,
      todo = todo,
      waitingFor = waitingFor.getOrElse(WaitForNothing()),
      currentTransaction = currentTransaction,
      visibleCalls = visibleCalls
    )

    def yieldState(): State = {
      val ls = newLocalState()
      state.copy(
        localStates = state.localStates + (invocationId -> ls)
      )
    }

    waitingFor = Some(WaitForFinishInvocation(AnyValue("nothing")))
    return yieldState()
  }


  def checkInvariantsLocal(state: State, localState: LocalState): Unit = {

    for (inv <- prog.invariants) {
      val e = evalExpr(inv.expr, localState, state)(defaultAnyValueCreator)
      if (e.value != true) {
        val e2 = evalExpr(inv.expr, localState, state)(tracingAnyValueCreator)
        throw new InvariantViolationException(inv, state, e2.info)
      }
    }
  }


  // TODO apply transaction (read own writes)
  def applyTransaction(currentTransaction: Option[TransactionInfo], inState: State): State = inState


  sealed abstract class AnyValueCreator[T <: AbstractAnyValue] {
    def apply(value: Any): T

    def apply(value: AnyValue): T = apply(value.value)

    def apply(value: Any, other: T): T = apply(value)

    def apply(value: Any, info: => EvalExprInfo, other: T): T = apply(value)
  }

  def defaultAnyValueCreator = new AnyValueCreator[AnyValue] {
    override def apply(value: Any): AnyValue = AnyValue(value)

    override def apply(value: AnyValue): AnyValue = value
  }

  def tracingAnyValueCreator = new AnyValueCreator[TracingAnyValue] {
    override def apply(value: Any): TracingAnyValue = TracingAnyValue(value)

    override def apply(value: AnyValue): TracingAnyValue = TracingAnyValue(value.value)

    override def apply(value: Any, other: TracingAnyValue): TracingAnyValue = TracingAnyValue(value, other.info)

    override def apply(value: Any, info: => EvalExprInfo, other: TracingAnyValue): TracingAnyValue = TracingAnyValue(value, info :: other.info)

  }

  def evalExpr[T <: AbstractAnyValue](expr: InExpr, localState: LocalState, inState: State)(implicit anyValueCreator: AnyValueCreator[T]): T = {
    try {
      evalExpr2(expr, localState, inState)
    } catch {
      case e: Exception => throw new Exception(s"Error evaluating $expr", e)
    }
  }

  def evalExpr2[T <: AbstractAnyValue](expr: InExpr, localState: LocalState, inState: State)(implicit anyValueCreator: AnyValueCreator[T]): T = {
    //    debugLog(s"executing expr $expr")
    //    debugLog(s"  vars = ${localState.varValues}")
    val state = applyTransaction(localState.currentTransaction, inState)

    expr match {
      case VarUse(source, typ, name) =>
        val varValue = localState.varValues.get(LocalVar(name)) match {
          case Some(value) =>
            if (value == null)
              throw new RuntimeException(s"Variable $name is null, vars = ${localState.varValues}.")
            value
          case None =>
            throw new RuntimeException(s"Variable $name not bound, vars = ${localState.varValues}.")
        }
        anyValueCreator(varValue)
      case BoolConst(_, _, value) =>
        anyValueCreator(value)
      case FunctionCall(source, typ, functionName, args) =>
        // TODO check if this is a query
        val eArgs: List[T] = args.map(evalExpr(_, localState, state))


        if (prog.programCrdt.hasQuery(functionName.name)) {
          val res: AnyValue = prog.programCrdt.evaluateQuery(functionName.name, eArgs, state)
          return anyValueCreator(res)
        }

        prog.findQuery(functionName.name) match {
          case Some(query) =>
            evaluateQueryDecl(query, eArgs, localState, state)
          case None =>
            anyValueCreator(DataTypeValue(functionName.name, eArgs.map(a => AnyValue(a.value))))
        }
      case ApplyBuiltin(source, typ, function, args) =>
        val eArgs = args.map(evalExpr(_, localState, state)(anyValueCreator))
        function match {
          case BF_isVisible() =>
            anyValueCreator(localState.visibleCalls.contains(eArgs.head.value.asInstanceOf[CallId]))
          case BF_happensBefore() if eArgs(0).value.isInstanceOf[CallId] =>
            val callId1 = eArgs(0).value.asInstanceOf[CallId]
            val callId2 = eArgs(1).value.asInstanceOf[CallId]
            val call1 = state.calls(callId1)
            val call2 = state.calls(callId2)
            anyValueCreator(
              localState.visibleCalls.contains(callId1)
                && localState.visibleCalls.contains(callId2)
                && call2.callClock.includes(call1)
            )
          case BF_happensBefore() =>
            val invoc1 = eArgs(0).value.asInstanceOf[InvocationId]
            val invoc2 = eArgs(1).value.asInstanceOf[InvocationId]

            var calls1 = Set[CallInfo]()
            var calls2 = Set[CallInfo]()

            for ((id, c) <- state.calls) {
              if (c.origin == invoc1)
                calls1 += c
              if (c.origin == invoc2)
                calls2 += c
            }
            val res = calls1.nonEmpty &&
              calls2.nonEmpty &&
              calls1.forall(c1 => calls2.forall(c2 => c1.happensBefore(c2)))

            anyValueCreator(res)
          case BF_sameTransaction() =>
            val callId1 = eArgs(0).value.asInstanceOf[CallId]
            val callId2 = eArgs(1).value.asInstanceOf[CallId]
            anyValueCreator(
              state.calls(callId1).callTransaction == state.calls(callId2).callTransaction
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
            //            if (expr.toString.contains("notFound") && !eArgs(0).value.toString.contains("NoResult")) {
            //              debugLog(s"     ${expr}")
            //              debugLog(s"     check ${eArgs(0).value} == ${eArgs(1).value}")
            //            }
            val leftValue: T = eArgs(0)
            val rightValue: T = eArgs(1)
            val comparison: Boolean = leftValue.value == rightValue.value
            anyValueCreator(comparison, EvalEqExprInfo(expr.getSource(), leftValue, rightValue), leftValue)
          case BF_notEquals() =>
            anyValueCreator(eArgs(0).value != eArgs(1).value)
          case BF_and() =>
            if (!eArgs(0).value.asInstanceOf[Boolean]) {
              anyValueCreator(false, EvalAndExprInfo(args(0).getSource(), Left()), eArgs(0))
            } else if (!eArgs(1).value.asInstanceOf[Boolean]) {
              anyValueCreator(false, EvalAndExprInfo(args(1).getSource(), Right()), eArgs(1))
            } else {
              anyValueCreator(true)
            }
          case BF_or() =>
            if (eArgs(0).value.asInstanceOf[Boolean]) {
              anyValueCreator(true, EvalOrExprInfo(args(0).getSource(), Left()), eArgs(0))
            } else if (eArgs(1).value.asInstanceOf[Boolean]) {
              anyValueCreator(true, EvalOrExprInfo(args(1).getSource(), Right()), eArgs(1))
            } else {
              anyValueCreator(false)
            }
          case BF_implies() =>
            if (!eArgs(0).value.asInstanceOf[Boolean]) {
              anyValueCreator(true, EvalImpliesExprInfo(args(0).getSource(), Left()), eArgs(0))
            } else if (!eArgs(1).value.asInstanceOf[Boolean]) {
              anyValueCreator(false, EvalImpliesExprInfo(args(1).getSource(), Right()), eArgs(1))
            } else {
              anyValueCreator(true)
            }
          case BF_not() =>
            anyValueCreator(!eArgs(0).value.asInstanceOf[Boolean], eArgs(0))
          case BF_getOperation() =>
            val info: CallInfo = state.calls(eArgs(0).value.asInstanceOf[CallId])
            anyValueCreator(info.operation)
          case BF_getInfo() =>
            val info: InvocationInfo = state.invocations(eArgs(0).value.asInstanceOf[InvocationId])
            anyValueCreator(info.operation)
          case BF_getResult() =>
            val info: InvocationInfo = state.invocations(eArgs(0).value.asInstanceOf[InvocationId])
            info.result.map(anyValueCreator(_)).getOrElse(anyValueCreator(DataTypeValue("NoResult", List())))
          case BF_getOrigin() =>
            val info: CallInfo = state.calls(eArgs(0).value.asInstanceOf[CallId])
            anyValueCreator(info.origin)
          case BF_inCurrentInvoc() =>
            ???
        }

      case QuantifierExpr(source, typ, Exists(), vars, e) =>

        // find variable assignment making this true
        val results = (for (m <- enumerate(vars, state)) yield {
          val newLocalState = localState.copy(varValues = localState.varValues ++ m)
          val r = evalExpr(e, newLocalState, state)(anyValueCreator)
          if (r.value.asInstanceOf[Boolean]) {
            return anyValueCreator(true, QuantifierInfo(source, m), r)
          }
          (m, r)
        }).force
        // no matching value exists
        anyValueCreator(false)
      //        var res = anyValueCreator(false)
      //        res match {
      //          case tres: TracingAnyValue =>
      //          val infos = tres.info
      //            for ((m,r) <- results) {
      //              res = anyValueCreator(false, QuantifierAllInfo(source, m, r.asInstanceOf[TracingAnyValue].info), res)
      //            }
      //          case _ =>
      //        }
      //        return res
      case QuantifierExpr(source, typ, Forall(), vars, e) =>

        // find variable assignment making this true
        val results = (for (m <- enumerate(vars, state)) yield {
          val newLocalState = localState.copy(varValues = localState.varValues ++ m)
          val r = evalExpr(e, newLocalState, state)(anyValueCreator)
          if (!r.value.asInstanceOf[Boolean]) {
            return anyValueCreator(false, QuantifierInfo(source, m), r)
          }
          (m, r)
        }).force

        // all values matching
        anyValueCreator(true)
      //        var res = anyValueCreator(true)
      //        res match {
      //          case tres: TracingAnyValue =>
      //            val infos = tres.info
      //            for ((m,r) <- results) {
      //              res = anyValueCreator(false, QuantifierAllInfo(source, m, r.asInstanceOf[TracingAnyValue].info), res)
      //            }
      //          case _ =>
      //        }
      //        return res
    }
  }

  def evaluateQueryDecl[T <: AbstractAnyValue](query: InQueryDecl, eArgs: List[T], localState: LocalState, state: State)(implicit anyValueCreator: AnyValueCreator[T]): T = {
    evaluateQueryDeclStream(query, eArgs, localState, state).headOption.getOrElse(anyValueCreator("invalid"))
  }

  def evaluateQueryDeclStream[T <: AbstractAnyValue](query: InQueryDecl, eArgs: List[T], localState: LocalState, state: State)(implicit anyValueCreator: AnyValueCreator[T]): Stream[T] = {
    val paramValues: Map[LocalVar, AnyValue] = query.params.zip(eArgs).map {
      case (param, value) => LocalVar(param.name.name) -> AnyValue(value.value)
    }.toMap

    var ls = localState.copy(
      varValues = paramValues
    )

    query.implementation match {
      case Some(impl) =>
        Stream(evalExpr(impl, ls, state))
      case None =>
        query.ensures match {
          case Some(ensures) =>
            println(ensures) // todo remove

            // try to find valid value
            val validValues = enumerateValues(query.returnType, state).filter(r => {
              println(s"trying result $r")
              val ls2 = ls.copy(
                varValues = ls.varValues + (LocalVar("result") -> r)
              )
              val check: T = evalExpr(ensures, ls2, state)
              check.value.asInstanceOf[Boolean]
            })
            // return first matching value or "invalid" if postcondition cannot be satisfied (should not happen for valid postconditions)
            validValues.map(anyValueCreator(_))
          case None =>
            // this is just a dummy implementation returning an arbitrary result
            //                enumerateValues(query.returnType, state).head
            Stream.empty
        }

    }
  }

  def enumerate(vars: List[InVariable], state: State): Stream[Map[LocalVar, AnyValue]] = vars match {
    case Nil =>
      List(Map[LocalVar, AnyValue]()).toStream
    case List(v) =>
      enumerateValues(v.typ, state).map(x => Map(LocalVar(v.name.name) -> x))
    case v :: tl =>
      val s = enumerateValues(v.typ, state).map(x => Map(LocalVar(v.name.name) -> x))
      s.flatMap(vals => {
        val tls = enumerate(tl, state)
        tls.map(vals2 => {
          vals ++ vals2
        })
      })
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
      state.calls.keys.toStream.map(AnyValue(_))
    case InvocationIdType() =>
      state.invocations.keys.toStream.map(AnyValue(_))
    case InvocationInfoType() =>
      ???
    case InvocationResultType() =>
      ???
    case SomeOperationType() =>
      ???
    case OperationType(name, source) =>
      ???
    case FunctionType(argTypes, returnType, source) =>
      ???
    case SimpleType(name, source) =>
      prog.findDatatype(name) match {
        case None =>
          (0 to domainSize).map(i => domainValue(name, i)).toStream
        case Some(dt) =>
          for (dtcase <- dt.dataTypeCases.toStream; params <- enumerate(dtcase.params, state)) yield {
            val args = dtcase.params.map(p => params(LocalVar(p.name.name)))
            AnyValue(DataTypeValue(dtcase.name.name, args))
          }
      }
    // TODO handle datatypes

    case idt@IdType(name, source) =>
      state.knownIds.getOrElse(idt, Set()).toStream
    case UnresolvedType(name, source) =>
      ???
  }


}

object Interpreter {


  class InvariantViolationException(val inv: InInvariantDecl, val state: State, val info: List[EvalExprInfo])
    extends RuntimeException(s"Invariant in line ${inv.source.getLine}") {
  }

  abstract class AbstractAnyValue {
    def value: Any
  }

  case class AnyValue(value: Any) extends AbstractAnyValue {
    override def toString: String = value.toString
  }

  case class TracingAnyValue(value: Any, info: List[EvalExprInfo] = List()) extends AbstractAnyValue {
    override def toString: String = value.toString
  }

  case class TransactionId(id: Int) {
    override def toString: String = s"tx_$id"
  }

  object TransactionId {
    implicit def orderById: Ordering[TransactionId] = Ordering.by(_.id)
  }


  case class CallId(id: Int) {
    override def toString: String = s"call_$id"
  }

  object CallId {
    implicit val ord: Ordering[CallId] = Ordering.by(_.id)
  }


  case class InvocationId(id: Int) {
    override def toString: String = s"invoc_$id"
  }

  object InvocationId {
    implicit val ord: Ordering[InvocationId] = Ordering.by(_.id)
  }


  case class DataTypeValue(operationName: String, args: List[AnyValue]) {
    override def toString: String = s"$operationName(${args.mkString(", ")})"
  }

  case class CallInfo(
    id: CallId,
    operation: DataTypeValue,
    callClock: SnapshotTime,
    callTransaction: TransactionId,
    origin: InvocationId
  ) {
    def happensBefore(c2: CallInfo) = c2.callClock.snapshot.contains(id)

    def happensAfter(c2: CallInfo) = this.callClock.snapshot.contains(c2.id)

    def happensParallel(c2: CallInfo) = !happensAfter(c2) && !happensBefore(c2)
  }

  case class SnapshotTime(snapshot: Set[CallId]) {
    def includes(call: CallInfo): Boolean = {
      snapshot.contains(call.id)
    }

    def happensBefore(other: SnapshotTime): Boolean = {
      SnapshotTimeOrder.lteq(this, other)
    }

    def happensAfter(other: SnapshotTime): Boolean = {
      SnapshotTimeOrder.gteq(this, other)
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
    origin: InvocationId,
    currentCalls: List[CallInfo],
    finished: Boolean
  ) {
    def happenedBefore(other: TransactionInfo) = {
      currentCalls.map(_.id).toSet.subsetOf(other.start.snapshot)
    }

  }

  def domainValue(name: String, i: Int) = {
    AnyValue(name + "_" + i)
  }


  case class InvocationInfo(
    id: InvocationId,
    operation: DataTypeValue,
    result: Option[AnyValue]
  )


  case class LocalVar(name: String) {
    override def toString: String = name
  }

  // local state for one invocation
  case class LocalState(
    varValues: Map[LocalVar, AnyValue],
    todo: List[StatementOrAction],
    waitingFor: LocalWaitingFor,
    currentTransaction: Option[TransactionInfo],
    visibleCalls: Set[CallId]
  ) {

    def isCallVisible(c: CallId): Boolean = {
      return visibleCalls.contains(c)
    }

    def isTransactionVisible(tx: TransactionId, state: State): Boolean = {
      visibleCalls.exists(c => state.calls(c).origin == tx)
    }

    override def toString: String =
      s"""
         |LocalState(
         |  varValues:
         |    ${varValues.toList.map { case (k, v) => s"$k -> $v" }.mkString("\n    ")}
         |  todo: ${todo.size}
         |  waitingFor: $waitingFor
         |  currentTransaction: $currentTransaction
         |  visibleCalls: $visibleCalls
         |)
       """.stripMargin
  }

  sealed abstract class LocalWaitingFor

  case class WaitForBegin() extends LocalWaitingFor

  case class WaitForNothing() extends LocalWaitingFor

  case class WaitForBeginTransaction() extends LocalWaitingFor

  case class WaitForFinishInvocation(result: AnyValue) extends LocalWaitingFor

  case class WaitForNewId(varname: String, typename: InTypeExpr) extends LocalWaitingFor


  sealed abstract class StatementOrAction

  case class ExecStmt(inStatement: InStatement) extends StatementOrAction

  case class EndAtomic() extends StatementOrAction


  // System state
  case class State(
    // the set of all calls which happened on the database
    calls: Map[CallId, CallInfo] = Map(),
    //    transactionCalls: Map[TransactionId, Set[CallId]] = Map(),
    maxCallId: Int = 0,
    transactions: Map[TransactionId, TransactionInfo] = Map(),
    maxTransactionId: Int = 0,
    invocations: Map[InvocationId, InvocationInfo] = Map(),
    maxInvocationId: Int = 0,
    // returned Ids for each id-type
    knownIds: Map[IdType, Set[AnyValue]] = Map(),
    localStates: Map[InvocationId, LocalState] = Map()

  ) {
    // only for faster evaluation:
    lazy val operationToCall: Map[DataTypeValue, CallId] =
      calls.values.map(ci => (ci.operation, ci.id)).toMap
  }


  // actions taken by the interpreter
  sealed trait Action {
    val invocationId: InvocationId
  }

  case class CallAction(
    invocationId: InvocationId,
    procname: String,
    args: List[AnyValue]
  ) extends Action

  case class LocalAction(
    invocationId: InvocationId,
    localAction: LocalStep
  ) extends Action

  case class InvariantCheck(
    invocationId: InvocationId
  ) extends Action


  sealed trait LocalStep

  case class StartTransaction(
    newTransactionId: TransactionId,
    pulledTransaction: Set[TransactionId]
  ) extends LocalStep

  case class Fail() extends LocalStep

  case class Return() extends LocalStep

  case class NewId(id: Int) extends LocalStep

  abstract class EvalExprInfo

  case class QuantifierInfo(source: SourceTrace, info: Map[LocalVar, AnyValue]) extends EvalExprInfo {
    override def toString: String = {
      val vars = for ((k, v) <- info) yield s"$k -> $v"
      s"Quantifier in line ${source.getLine} instantiated with ${vars.mkString(", ")}"
    }
  }

  case class QuantifierAllInfo(source: SourceTrace, info: Map[LocalVar, AnyValue], chain: List[EvalExprInfo]) extends EvalExprInfo {
    override def toString: String = {
      val vars = for ((k, v) <- info) yield s"$k -> $v"
      s"Quantifier in line ${source.getLine} instantiated with ${vars.mkString(", ")} -> { ${chain.mkString(", ")}"
    }
  }

  sealed abstract class Side

  case class Left() extends Side {
    override def toString: String = "left"
  }

  case class Right() extends Side {
    override def toString: String = "right"
  }

  case class EvalAndExprInfo(source: SourceTrace, side: Side) extends EvalExprInfo {
    override def toString: String = {
      s"Conjunction in line ${source.getLine} was false on the $side side"
    }
  }

  case class EvalOrExprInfo(source: SourceTrace, side: Side) extends EvalExprInfo {
    override def toString: String = {
      s"Disjunction in line ${source.getLine} was true on the $side side"
    }
  }

  case class EvalImpliesExprInfo(source: SourceTrace, side: Side) extends EvalExprInfo {
    override def toString: String = side match {
      case Left() => s"Assumption of implication in line ${source.getLine} was false"
      case Right() => s"Right side of implication in line ${source.getLine} was false"
    }
  }

  case class EvalEqExprInfo(source: SourceTrace, left: AbstractAnyValue, right: AbstractAnyValue) extends EvalExprInfo {
    override def toString: String = {
      s"Equality check was false: $left != $right"
    }
  }


}

