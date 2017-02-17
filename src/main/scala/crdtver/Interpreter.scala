package crdtver

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javax.print.attribute.standard.MediaSize.Other

import crdtver.InputAst._
import crdtver.Repliss.{QuickcheckCounterexample, ReplissResult}

import scala.collection.immutable.{::, Nil}
import scala.util.Random


class Interpreter(prog: InProgram) {

  val debug = false

  def debugLog(s: String): Unit = {
    if (debug) {
      println(s)
    }
  }


  // custom data types can have values 0 <= x < domainSize
  val domainSize = 3

  // maximum number of known ids for generating random values
  val maxUsedIds = 1

  case class AnyValue(value: Any) {
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

  case class InvocationId(id: Int) {
    override def toString: String = s"invoc_$id"
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

  }

  case class SnapshotTime(snapshot: Set[CallId]) {
    def includes(call: CallInfo): Boolean = {
      snapshot.contains(call.id)
    }

    def happensBefore(other: SnapshotTime): Boolean = {
      SnapshotTimeOrder.lteq(this, other)
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


  sealed abstract class ActionProvider {
    def discardLast(): Unit

    def nextAction(state: State): Option[Action]
  }

  class TraceActionProvider(trace: List[Action]) extends ActionProvider {
    private var exeutedActions = List[Action]()
    private var maxGivenId = 0
    private var todo = trace

    def getTrace(): List[Action] = exeutedActions.reverse

    override def discardLast() = {
      debugLog(s"### discarding ${exeutedActions.head}")
      exeutedActions = exeutedActions.tail
    }

    override def nextAction(state: State): Option[Action] = {
      todo match {
        case Nil => None
        case action1 :: todo2 =>
          val action = action1 match {
            case InvariantCheck(invocationId) =>
              if (state.localStates.contains(invocationId)) {
                debugLog(s"### keeping check at $invocationId")
                action1
              } else {
                val alternative = state.localStates.keys.headOption.getOrElse(invocationId)
                debugLog(s"### changing from $invocationId to $alternative")
                InvariantCheck(alternative)
              }
            case _ => action1
          }

          exeutedActions = action :: exeutedActions
          todo = todo2
          Some(action)
      }
    }
  }

  class RandomActionProvider(limit: Int = 1000, seed: Int = 0) extends ActionProvider {
    // trace of executed actions,  newest actions first
    private var exeutedActions = List[Action]()
    private val rand = new Random(seed)
    private var maxGivenId = 0

    def getTrace(): List[Action] = exeutedActions.reverse

    override def discardLast() = {
      exeutedActions = exeutedActions.tail
    }

    override def nextAction(state: State): Option[Action] = {
      if (exeutedActions.size > limit) {
        return None
      }
      val action = randomAction(state)
      exeutedActions = action :: exeutedActions
      debugLog(s"generating action ${exeutedActions.size}/$limit")
      Some(action)
    }


    def newRandomInvariantCheck(state: State): Action = {
      val id = pickRandom(state.localStates.keys.toList)(rand)
      InvariantCheck(id)
    }

    def randomAction(state: State): Action = {


      val waitFors = getLocalWaitingFors(state)
      val i = rand.nextInt(waitFors.size + 1)
      if (i >= waitFors.size) {
        if (state.localStates.nonEmpty && rand.nextDouble() < 0.5) {
          newRandomInvariantCheck(state)
        } else {
          newRandomInvoaction(state)
        }
      } else {
        val (invoc, waitingFor) = waitFors(i)
        makeAction(state, invoc, waitingFor)
      }
    }


    def getPulledTransactions2(state: State, invoc: InvocationId): Set[TransactionId] = {
      val allTransactions = state.transactions.keySet

      // first: determine how many transactions to base this on:
      val count = 1 + rand.nextInt(2)

      val ls: LocalState = state.localStates.getOrElse(invoc, return Set())


      // get transactions, which are not yet in current snapshot
      val pullableTransactions = allTransactions.filter(tx => !ls.isTransactionVisible(tx, state))

      randomSizedSubsetPriority(pullableTransactions, count)
    }

    def makeAction(state: State, invoc: InvocationId, waitingFor: LocalWaitingFor): Action = waitingFor match {
      case WaitForBeginTransaction() =>

        val pulledTransactions1: Set[TransactionId] = getPulledTransactions2(state, invoc)
        //        val pulledTransactions = randomSubset(allTransactions, propability = 0.3)

        // add dependencies =
        var pulledTransactions = pulledTransactions1
        for (tx <- state.transactions.values) {
          if (pulledTransactions1.exists(tx2 => tx.happenedBefore(state.transactions(tx2)))) {
            pulledTransactions += tx.id
          }
        }


        LocalAction(invoc,
          StartTransaction(
            newTransactionId = TransactionId(state.maxTransactionId + 1),
            pulledTransaction = pulledTransactions
          )
        )
      case WaitForFinishInvocation(_) =>
        LocalAction(invoc, Return())
      case WaitForNewId(v, t) =>
        maxGivenId += 1
        LocalAction(invoc, NewId(maxGivenId))
      case WaitForBegin() | WaitForNothing() =>
        throw new RuntimeException("not possible")
    }


    def getPulledTransactions(state: State, allTransactions: Set[TransactionId]): Set[TransactionId] = {
      val transactionsWithSize = for (tx <- allTransactions) yield {
        val size = state.calls.find(_._2.callTransaction == tx) match {
          case Some((cid, cInfo)) => cInfo.callClock.snapshot.size
          case None => 0
        }
        (tx, size)
      }
      val pulledTransactions = randomSubsetW(transactionsWithSize, propability = 0.7)
      pulledTransactions
    }

    def randomValue(typ: InTypeExpr, knownIds: Map[IdType, Set[AnyValue]]): Option[AnyValue] = {
      typ match {
        case SimpleType(name, source) =>
          // TODO handle datatypes
          Some(domainValue(name, rand.nextInt(domainSize)))
        case idt@IdType(name, source) =>
          knownIds.get(idt) match {
            case Some(s) =>
              val ids = s.toList.sortBy(_.toString())
              Some(pickRandom(ids)(rand))
            case None => None
          }
        case BoolType() =>
          Some(AnyValue(rand.nextBoolean()))
        case IntType() =>
          Some(AnyValue(rand.nextInt(100)))
        case CallIdType() =>
          ???
        case InvocationIdType() =>
          ???
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
        case AnyType() =>
          ???
        case UnknownType() =>
          ???
        case UnresolvedType(name, source) =>
          ???
      }
    }

    def newRandomInvoaction(state: State): Action = {
      val invocId = InvocationId(state.maxInvocationId + 1)
      var tries = 0
      while (tries < 1000) {
        tries += 1
        var procedures = prog.procedures
        if (state.knownIds.size >= maxUsedIds) {
          // don't call more functions returning id-types
          procedures = procedures.filter(p => p.returnType match {
            case Some(IdType(name, source)) =>
              false
            case _ => true
          })
        }

        val proc = pickRandom(procedures)(rand)

        val args: List[Option[AnyValue]] = for (param <- proc.params) yield {
          randomValue(param.typ, state.knownIds)
        }
        if (!args.contains(None)) {
          return CallAction(invocId, proc.name.name, args.map(_.get))
        }
      }
      // this can happen if all functions require an id-type
      throw new RuntimeException("Could not find suitable arguments for any function")
    }


    def randomSubset[T](set: Set[T], propability: Double = 0.5): Set[T] = {
      set.filter(e => rand.nextDouble() < propability)
    }

    def randomSizedSubset[T](set: Set[T], count: Int): Set[T] = {
      if (set.isEmpty || count <= 0) {
        Set()
      } else {
        val ar = set.toList
        val index = rand.nextInt(ar.size)
        val elem = ar(index)
        randomSubset(set - elem, count - 1) + elem
      }
    }

    def randomElementFromPriorityList[T](list: List[T]): T = list match {
      case Nil => throw new IllegalArgumentException("empty list")
      case List(x) => x
      case (x :: xs) =>
        if (rand.nextDouble() < 0.5) {
          x
        } else {
          randomElementFromPriorityList(xs)
        }
    }

    // random subset with size, but preferring small elements
    def randomSizedSubsetPriority[T](set: Set[T], count: Int)(implicit ordering: Ordering[T]): Set[T] = {
      if (set.isEmpty || count <= 0) {
        Set()
      } else {
        val ar: List[T] = set.toList.sorted(ordering)
        val elem = randomElementFromPriorityList(ar)
        randomSubset(set - elem, count - 1) + elem
      }
    }


    def randomSubsetW[T](set: Set[(T, Int)], propability: Double = 0.5): Set[T] = {
      if (set.isEmpty) {
        return Set()
      }
      val wmax = set.map(_._2).max
      val s = set.filter { case (_, w) =>
        val probability2 = propability * ((1.0 + wmax - w) / wmax)
        rand.nextDouble() < probability2
      }

      return s.map(_._1)
    }

    private def getLocalWaitingFors(state: State): List[(InvocationId, LocalWaitingFor)] = {
      val result = for ((invocation, ls) <- state.localStates) yield {
        (invocation, ls.waitingFor)
      }
      result.toList
    }

  }

  def pickRandom[T](list: List[T])(implicit rand: Random): T = {
    if (list.isEmpty)
      throw new IllegalArgumentException("List empty")
    val i = rand.nextInt(list.size)
    list(i)
  }


  private def domainValue(name: String, i: Int) = {
    AnyValue(name + "_" + i)
  }


  def renderStateGraph(state: State): (String,String) = {
    val sb = new StringBuilder()

    def p(s: String): Unit = {
      sb.append(s)
      sb.append("\n")
    }

    printStateGraph(state, p)
    val dot = sb.toString()
    val dotIs = new ByteArrayInputStream(dot.getBytes(StandardCharsets.UTF_8))
    import sys.process._

    val reducedDot: String = ("tred" #< dotIs).!!

    val dotIs2 = new ByteArrayInputStream(reducedDot.getBytes(StandardCharsets.UTF_8))

    val output: String = ("dot -Tsvg" #< dotIs2).!!
    debugLog(s"SVG output = $output")
    return (reducedDot, output)
  }


  def printStateGraphToFile(state: State, filename: String): Unit = {
    val sb = new StringBuilder()

    def p(s: String): Unit = {
      sb.append(s)
      sb.append("\n")
    }

    printStateGraph(state, p)

    Files.write(Paths.get(s"model/graph_$filename.dot"), sb.toString().getBytes(StandardCharsets.UTF_8))
    import sys.process._
    s"tred model/graph_$filename.dot" #| s"dot -Tsvg -o model/graph_$filename.svg" !
  }

  def printStateGraph(state: State, p: (String) => Unit): Unit = {

    p(s"digraph G {")
    for (i <- state.invocations.values) {
      var containsCall = false
      p(
        s"""subgraph cluster_${i.id} {
           |   style="rounded,filled";
           |   color="cadetblue3:cadetblue4";
           |   node [style=filled, color=white]
           |   label = "${i.id}
           |      ${i.operation}
           |      result: ${i.result}"
         """.stripMargin)
      val txns = state.transactions.values.filter(_.origin == i.id)

      state.localStates.get(i.id).map(_.currentTransaction) match {
        case Some(tx) =>
          p(s"/* current tx = ${tx} */")
        case None =>
          p(s"/* no current tx */")
      }

      for (tx <- txns) {
        p(
          s"""subgraph cluster_${tx.id} {
             |   style="rounded,filled";
             |   color="cadetblue2:cadetblue3";
             |   node [style=filled, color=white, shape=box, style="rounded,filled"]
             |   label = "${tx.id}"
                 """.stripMargin)
        val calls = state.calls.values.filter(_.callTransaction == tx.id)
        p(s"/* ${tx.id} calls = ${calls} */")
        p(s"/* ${tx.id} currentCalls = ${tx.currentCalls} */")
        for (c <- calls) {
          val opStr =
            if (c.operation.operationName.startsWith("queryop_")) {
              val op = DataTypeValue(
                operationName = c.operation.operationName.drop("queryop_".length),
                args = c.operation.args.take(c.operation.args.size - 1)
              )
              val res = c.operation.args.last
              s"$op\nresult: $res"
            } else {
              c.operation.toString
            }

          p(s"""${c.id}[label="${c.id}\n$opStr"];""")
          containsCall = true
        }
        p(s"""}""")
      }
      if (!containsCall) {
        p(s"empty_${i.id}")
      }

      p("}")
    }

    // add dependencies
    for (c <- state.calls.values) {
      for (dep <- c.callClock.snapshot) {
        p(s"${dep} -> ${c.id};")
      }
      p("")
    }
    p("}")


  }


  def printTrace(trace: List[Action]): String = {
    val sb = new StringBuilder
    for (action <- trace) {
      sb.append(s"$action\n")
    }
    sb.toString()
  }

  def randomTests(limit: Int = 100, seed: Int = 0, debug: Boolean = false): Option[QuickcheckCounterexample] = {
    var startTime = System.nanoTime();

    val ap = new RandomActionProvider(limit, seed)
    try {
      val state = execute(ap)
      val trace = ap.getTrace()
      debugLog(s"Executed ${state.invocations.size} invocations in ${(System.nanoTime() - startTime)/1000000}ms and found no counterexample.")

      if (debug) {
        for (action <- trace) {
          debugLog("  " + action)
        }
        printStateGraphToFile(state, "success")
      }
      None
    } catch {
      case e: InvariantViolationException =>
        val trace = ap.getTrace()


        debugLog(s"Found counter example with ${e.state.invocations.size} invocations in ${(System.nanoTime() - startTime)/1000000}ms")

        startTime = System.nanoTime()
        val (smallTrace, smallState) = tryShrink(trace, e.state)
        debugLog(s"Reduced to example with ${smallState.invocations.size} invocations in ${(System.nanoTime() - startTime)/1000000}ms")

        if (debug) {
          for (action <- smallTrace) {
            debugLog("  " + action)
          }
          debugLog(s"reduced from ${trace.size} to ${smallTrace.size} actions")
          printStateGraphToFile(e.state, "original")
          printStateGraphToFile(smallState, "shrunk")
        }

        val (dot, svg) = renderStateGraph(smallState)

        Some(QuickcheckCounterexample(
          brokenInvariant = e.inv.source.range,
          trace = printTrace(smallTrace),
          counterExampleSvg = svg,
          counterExampleDot = dot
        ))
    }
  }

  def removeAtIndex[T](l: List[T], index: Int): List[T] =
    l.take(index) ++ l.drop(index + 1)

  // @tailrec TODO why not?
  private def tryShrink(trace: List[Action], lastState: State): (List[Action], State) = {
    debugLog(s"shrinking trace with ${trace.length} elements")
    for (i <- trace.indices) {
      val shrunkTrace = removeAtIndex(trace, i)
      debugLog(s"Trying to remove action ${trace(i)}")
      tryShrunkTrace(shrunkTrace) match {
        case Some((tr, s)) =>
          debugLog("Shrinking successful")
          return tryShrink(tr, s)
        case None =>
          debugLog("Shrinking failed")
        // continue
      }
    }
    (trace, lastState)
  }

  def tryShrunkTrace(trace: List[Action]): Option[(List[Action], State)] = {
    val ap = new TraceActionProvider(trace)
    try {
      val state = execute(ap)
      None
    } catch {
      case e: InvariantViolationException =>
        Some((ap.getTrace(), e.state))
    }
  }


  def execute(actionProvider: ActionProvider): State = {
    var state = State()

    while (true) {
      val action = actionProvider.nextAction(state)
      if (action.isEmpty) {
        return state
      }
      executeAction(state, action.get) match {
        case Some(s) =>
          state = s
        case None =>
          actionProvider.discardLast()
      }
    }
    return state
  }

  def findProcedure(procname: String): InProcedure =
    prog.procedures.find(p => p.name.name == procname)
      .getOrElse(throw new RuntimeException(s"Procedure $procname not found."))

  def findQuery(queryName: String): Option[InQueryDecl] =
    prog.queries.find(p => p.name.name == queryName)

  def findType(name: String): Option[InTypeDecl] =
    prog.types.find(t => t.name.name == name)

  def findDatatype(name: String): Option[InTypeDecl] =
    findType(name).find(t => t.dataTypeCases.nonEmpty)


  def happensBefore(state: State, c1: CallId, c2: CallId): Boolean = {
    val ci1 = state.calls(c1)
    val ci2 = state.calls(c2)
    ci1.callClock.happensBefore(ci2.callClock)
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
            Some(state)
          case None => None
        }
      case CallAction(invocationId, procname, args) =>
        if (state.invocations.contains(invocationId)) {
          // already has invocation with this key
          return None
        }
        val proc = findProcedure(procname)
        val varvalues = (for ((param, arg) <- proc.params.zip(args)) yield {
          LocalVar(param.name.name) -> arg
        }).toMap
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

                val returnType = findProcedure(newInvocationInfo.operation.operationName).returnType

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
                  varValues = localState.varValues + (LocalVar(varname) -> AnyValue(s"${typename}_$id"))
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
            val condVal = evalExpr(cond, newLocalState(), state).value
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
              operation = DataTypeValue(functionName.name, args.map(evalExpr(_, newLocalState(), state))),
              callClock = SnapshotTime(visibleCalls),
              callTransaction = currentTransaction.get.id,
              origin = invocationId
            )
            currentTransaction = currentTransaction.map(tr => tr.copy(
              currentCalls = tr.currentCalls :+ newCallInfo
            ))

          case Assignment(source, varname, expr) =>
            val e = evalExpr(expr, newLocalState(), state)
            varValues = varValues + (LocalVar(varname.name) -> e)
          case NewIdStmt(source, varname, typename) =>
            waitingFor = Some(WaitForNewId(varname.name, typename))
            return yieldState()
          case ReturnStmt(source, expr, assertions) =>
            waitingFor = Some(WaitForFinishInvocation(evalExpr(expr, newLocalState(), state)))
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
      val e = evalExpr(inv.expr, localState, state)
      if (e.value != true) {
        throw new InvariantViolationException(inv, state)
      }
    }
  }

  class InvariantViolationException(val inv: InInvariantDecl, val state: State)
    extends RuntimeException(s"Invariant in line ${inv.source.getLine}") {


  }


  // TODO apply transaction (read own writes)
  def applyTransaction(currentTransaction: Option[TransactionInfo], inState: State): State = inState

  def evalExpr(expr: InExpr, localState: LocalState, inState: State): AnyValue = {
    //    debugLog(s"executing expr $expr")
    //    debugLog(s"  vars = ${localState.varValues}")

    val state = applyTransaction(localState.currentTransaction, inState)

    expr match {
      case VarUse(source, typ, name) =>
        localState.varValues(LocalVar(name))
      case FunctionCall(source, typ, functionName, args) =>
        // TODO check if this is a query
        val eArgs: List[AnyValue] = args.map(evalExpr(_, localState, state))

        findQuery(functionName.name) match {
          case Some(query) =>
            query.implementation match {
              case Some(impl) =>
                val ls = localState.copy(
                  varValues = localState.varValues ++
                    query.params.zip(eArgs).map { case (param, value) => LocalVar(param.name.name) -> value }.toMap
                )


                evalExpr(impl, ls, state)
              case None =>
                // TODO get result based on axioms
                // this is just a dummy implementation returning an arbitrary result
                //                enumerateValues(query.returnType, state).head
                AnyValue("???")

            }
          case None =>
            AnyValue(DataTypeValue(functionName.name, eArgs))
        }
      case ApplyBuiltin(source, typ, function, args) =>
        val eArgs = args.map(evalExpr(_, localState, state))
        function match {
          case BF_isVisible() =>
            AnyValue(localState.visibleCalls.contains(eArgs.head.value.asInstanceOf[CallId]))
          case BF_happensBefore() if eArgs(0).value.isInstanceOf[CallId] =>
            val callId1 = eArgs(0).value.asInstanceOf[CallId]
            val callId2 = eArgs(1).value.asInstanceOf[CallId]
            val call1 = state.calls(callId1)
            val call2 = state.calls(callId2)
            AnyValue(
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

            AnyValue(res)
          case BF_sameTransaction() =>
            val callId1 = eArgs(0).value.asInstanceOf[CallId]
            val callId2 = eArgs(1).value.asInstanceOf[CallId]
            AnyValue(
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
            info.result.getOrElse(AnyValue(DataTypeValue("NoResult", List())))
          case BF_getOrigin() =>
            val info: CallInfo = state.calls(eArgs(0).value.asInstanceOf[CallId])
            AnyValue(info.origin)
          case BF_inCurrentInvoc() =>
            ???
        }

      case QuantifierExpr(source, typ, quantifier, vars, e) =>
        val res = quantifier match {
          case Forall() =>
            enumerate(vars, state).forall(m => evalExpr(e, localState.copy(varValues = localState.varValues ++ m), state).value.asInstanceOf[Boolean])
          case Exists() =>
            enumerate(vars, state).exists(m => evalExpr(e, localState.copy(varValues = localState.varValues ++ m), state).value.asInstanceOf[Boolean])
        }
        AnyValue(res)
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
      findDatatype(name) match {
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

object InterpreterTest {
  def main(args: Array[String]): Unit = {
    //    val input = Helper.getResource("/examples/userbase_fail3.rpls")
    val input = Helper.getResource("/examples/userbase_fail1.rpls")
    //    val input = Helper.getResource("/examples/tournament.rpls")
    val typed = Repliss.parseAndTypecheck(input)
    val prog = AtomicTransform.transformProg(typed.get())

    println("prog: ---")
    println(prog.procedures.map(p => s"$p\n${p.body}\n\n").mkString("\n\n"))
    println("-----")


    println("tests start")
    val interpreter = new Interpreter(prog)
    interpreter.randomTests(limit = 200)

    println("tests done")
  }
}