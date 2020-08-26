package crdtver.testing

import java.util.Objects

import crdtver.Repliss.QuickcheckCounterexample
import crdtver.RunArgs
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionType, IdType, InProgram, InTypeExpr, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType, _}
import crdtver.testing.Interpreter._
import crdtver.utils.{LazyListUtils, TimeTaker}
import crdtver.utils.LazyListUtils.LazyListExtensions

import scala.collection.immutable.{::, Nil}
import scala.collection.mutable
import scala.ref.WeakReference
import scala.util.hashing.MurmurHash3

/**
 * This class is responsible for executing random tests on a Repliss program.
 */
class SmallcheckTester2(prog: InProgram, runArgs: RunArgs) {

  // custom data types can have values 0 <= x < domainSize
  private val domainSize = 3

  // maximum number of known ids for generating random values
  private val maxUsedIds = 2

  private val interpreter = new Interpreter(prog, runArgs, domainSize)


  private val debug = false

  private val timeTaker = new TimeTaker(true)

  private def debugLog(s: String): Unit = {
    if (debug) {
      println(s)
    }
  }

  private sealed abstract class ActionProvider {
    def discardLast(): Unit

    def nextAction(state: State): Option[Action]
  }

  private class TraceActionProvider(trace: List[Action]) extends ActionProvider {
    private var exeutedActions = List[Action]()
    private var maxGivenId = 0
    private var todo = trace

    private def getTrace(): List[Action] = exeutedActions.reverse

    override def discardLast(): Unit = {
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


  private def possibleActions(state: State, sequentialMode: Boolean): LazyList[Action] = {

    val waitFors: LazyList[(InvocationId, LocalWaitingFor)] = getLocalWaitingFors(state)
    val localActions: LazyList[Action] =
      for ((invoc, waitingFor) <- waitFors; a <- makeAction(state, invoc, waitingFor)) yield a

    //    val invChecks: LazyList[Action] = newRandomInvariantCheck(state)

    val invocations: LazyList[Action] =
      if (sequentialMode && localActions.nonEmpty)
      // first finish local actions before starting new invocations
        LazyList()
      else
        newRandomInvoaction(state)

    localActions ++ invocations
  }


  private def makeAction(state: State, invoc: InvocationId, waitingFor: LocalWaitingFor): LazyList[Action] = waitingFor match {
    case WaitForBeginTransaction() =>
      for (pulledTransactions: Set[TransactionId] <- TestingHelper.getPulledTransactions2(state, invoc)) yield
        LocalAction(invoc,
          StartTransaction(
            newTransactionId = TransactionId(state.maxTransactionId + 1),
            pulledTransaction = pulledTransactions
          )
        )
    case WaitForFinishInvocation(_) =>
      LazyList(LocalAction(invoc, Return()))
    case WaitForNewId(_, t) =>
      val id = state.knownIds.getOrElse(t, Map()).size
      LazyList(LocalAction(invoc, NewId(id)))
    case WaitForBegin() | WaitForNothing() =>
      throw new RuntimeException("not possible")
  }


  private def randomValue(typ: InTypeExpr, knownIds: Map[IdType, Map[AnyValue, InvocationId]]): LazyList[AnyValue] = {
    typ match {
      case SimpleType(name, typeArgs) =>
        // TODO handle datatypes
        // TODO substitute typeArgs
        for (i <- LazyList.range(1, domainSize) #::: LazyList(0)) yield
          Interpreter.domainValue(name, i)
      case idt@IdType(_name) =>
        // TODO should include generatedIds
        knownIds.get(idt) match {
          case Some(s) =>
            // only pick from the first N (maxUsedIds) unique identifiers to make it more likely that we work on the same data:
            s.keys.to(LazyList).take(maxUsedIds)
          case None =>
            LazyList()
        }
      case BoolType() =>
        LazyList(false, true).map(AnyValue)
      case IntType() =>
        for (i <- (0 until 100).to(LazyList)) yield
          AnyValue(i)
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
      case OperationType(name) =>
        ???
      case FunctionType(argTypes, returnType, source) =>
        ???
      case AnyType() =>
        ???
      case t: TransactionIdType =>
        ???
      case CallInfoType() => ???
      case t: TypeVarUse =>
        throw new RuntimeException(s"Cannot enumerate type variable $t")
      case _: UnitType => LazyList(AnyValue(()))
    }
  }

  private def newRandomInvoaction(state: State): LazyList[Action] = {
    val invocId = InvocationId(state.maxInvocationId + 1)
    for {
      proc <- new LazyListExtensions[InProcedure](prog.procedures.to(LazyList)).breadthFirst
      args <- LazyListUtils.allCombinations(proc.params.map(param => randomValue(param.typ, state.knownIds)))
    } yield CallAction(invocId, proc.name.name, args.toList)
  }


  private def getLocalWaitingFors(state: State): LazyList[(InvocationId, LocalWaitingFor)] = {
    val result = for ((invocation, ls) <- state.localStates) yield {
      (invocation, ls.waitingFor)
    }
    result.to(LazyList)
  }


  private def printTrace(trace: List[Action]): String = {
    val sb = new StringBuilder
    for (action <- trace) {
      sb.append(s"$action\n")
    }
    sb.toString()
  }

  /**
   * The actions denote the path taken:
   * at step n take action number actions(n)
   */
  case class Trace(
    actions: List[Int]
  ) {
    def add(i: Int): Trace = copy(actions = actions :+ i)

  }

  class HashState(
    val hash: Int,
    val actions: Trace,
    state: State
  ) {

    def getState: State = state

    //    def getStateI: State = {
    //      cache.get match {
    //        case Some(s) => s
    //        case None =>
    //          val s = timeTaker.measure("recover state")(() => executeTrace(initialState, actions).get.state)
    //          cache = new WeakReference[State](s)
    //          s
    //      }
    //    }
  }

  object HashState {
    def fromState(s: State, trace: Trace): HashState = {
      new HashState(hash(s), trace, s)
    }

    def hashState(s: State): Int = hash(s)

    def hash[T](t: T)(implicit h: Hashable[T]): Int =
      h.hash(t)

    trait Hashable[T] {
      def hash(t: T): Int
    }

    implicit def hashState: Hashable[State] =
      s => combineHashs(List(
        hash(s.calls),
        s.maxCallId,
        hash(s.transactions),
        s.maxTransactionId,
        hash(s.invocations),
        s.maxInvocationId,
        s.knownIds.size,
        hash(s.generatedIds),
        hash(s.localStates)
      ))

    implicit def hashMap[K, V](implicit kh: Hashable[K], vh: Hashable[V]): Hashable[Map[K, V]] =
      m => unorderedHash(4123,
        m.iterator.map { case (k, v) => hash(k) + hash(v) }
      )

    implicit def hashSet[T](implicit h: Hashable[T]): Hashable[Set[T]] =
      m => unorderedHash(781231323,
        m.iterator.map(hash(_))
      )

    implicit def hashList[T](implicit h: Hashable[T]): Hashable[List[T]] =
      m => combineHashs(
        Iterable(981923, m.size) ++ m.iterator.map(hash(_))
      )

    implicit def hashOption[T](implicit h: Hashable[T]): Hashable[Option[T]] =
      m => combineHashs(
        Iterable(if (m.isDefined) 96824 else 42356) ++ m.iterator.map(hash(_))
      )

    implicit def hashCallId: Hashable[CallId] = c => 0

    implicit def hashInvocationId: Hashable[InvocationId] = c => 0

    implicit def hashTransactionId: Hashable[TransactionId] = c => 0

    implicit def hashCallInfo: Hashable[CallInfo] = { info =>
      combineHashs(
        hash(info.operation),
        hash(info.callClock)
      )
    }

    implicit def hashTransactionInfo: Hashable[TransactionInfo] = { info =>
      combineHashs(
        hash(info.start),
        hash(info.currentCalls),
        hash(info.finished)
      )
    }

    implicit def hashInvocationInfo: Hashable[InvocationInfo] = { info =>
      combineHashs(
        hash(info.operation),
        hash(info.result)
      )
    }

    implicit def hashLocalState: Hashable[LocalState] = { info =>
      combineHashs(
        hash(info.varValues),
        hash(info.todo),
        hash(info.waitingFor),
        hash(info.currentTransaction),
        hash(info.visibleCalls)
      )
    }

    implicit def hashStatementOrAction: Hashable[StatementOrAction] = _.hashCode()


    implicit def hashLocalWaitingFor: Hashable[LocalWaitingFor] = {
      case w@WaitForBegin() => w.hashCode()
      case w@WaitForNothing() => w.hashCode()
      case w@WaitForBeginTransaction() => w.hashCode()
      case WaitForFinishInvocation(result) => 7123 + hash(result)
      case w@(id: WaitForNewId) => w.hashCode()
    }

    implicit def hashIdType: Hashable[IdType] = _.hashCode()

    implicit def hashLocalVar: Hashable[Interpreter.LocalVar] = _.hashCode()

    implicit def hashSnapshotTime: Hashable[SnapshotTime] = { v =>
      v.snapshot.size
    }

    implicit def hashDataTypeValue: Hashable[DataTypeValue] = { v =>
      combineHashs(
        Iterable(hash(v.operationName)) ++ v.args.map(hash(_))
      )
    }

    implicit def hashString: Hashable[String] = _.hashCode

    implicit def hashBoolean: Hashable[Boolean] = _.hashCode()

    implicit def hashAnyValue: Hashable[AnyValue] = { av =>
      av.value match {
        case d: DataTypeValue => hash(d)
        case d: DomainValue =>
          hash(d.name)
        case x: String =>
          x.hashCode()
        case x: Boolean =>
          x.hashCode()
        case x =>
          println(s"unhandled AnyValue $x (${x.getClass})")
          0
      }
    }


    def combineHashs(l: Int*): Int = combineHashs(l)


    def combineHashs(l: IterableOnce[Int]): Int = {
      var result = 1
      l.iterator.foreach { element =>
        result = 31 * result + element
      }
      result
    }

    def unorderedHash(seed: Int, xs: Int*): Int =
      unorderedHash(seed, xs)

    def unorderedHash(seed: Int, xs: IterableOnce[Int]): Int = {
      var a, b, n = 0
      var c = 1
      xs.iterator foreach { h =>
        a += h
        b ^= h
        c *= h | 1
        n += 1
      }
      var h = seed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      MurmurHash3.finalizeHash(h, n)
    }

  }


  case class S(state: State, trace: List[Action], ive: Option[InvariantViolationException] = None)

  /**
   * execute a trace starting from the given starting state
   */
  private def executeTrace(state: State, trace: Trace): Option[S] = {
    trace.actions match {
      case List() => Some(S(state, List(), None))
      case i :: rest =>
        val action = possibleActions(state, true)(i)
        executeAction(state, action) match {
          case Some(s) =>
            executeTrace(s.state, Trace(rest))
              .map(s2 => s2.copy(trace = action :: s2.trace))
          case None =>
            None
        }
    }
  }

  private def executeAction(state: State, action: Action): Option[S] = {
    try {
      interpreter.executeAction(state, action) match {
        case Some(newState) =>
          try {
            interpreter.checkInvariants(newState)
            Some(S(newState, List(action)))
          } catch {
            case ive: InvariantViolationException =>
              Some(S(newState, List(action), Some(ive)))
          }
        case None =>
          None
      }
    } catch {
      case ive: InvariantViolationException =>
        Some(S(state, List(action), Some(ive)))
      case e: Throwable =>
        val msg =
          s"""
             |Error ${e.getMessage}
             |when evaluating $action
             |in state ${state}"
             |""".stripMargin
        throw new RuntimeException(msg, e)
    }

  }


  object StateEq {
    def statesEquivalent(state1: State, state2: State): Boolean =
      findMatches(state1, state2, emptySubst).nonEmpty

    def emptySubst: Subst = Subst(XMapX(), XMapX(), XMapX(), XMapX())

    def findMatches[T](a: T, b: T, s: Subst)(implicit m: Match[T]): LazyList[Any] =
      m.findMatches(a, b, s)

    case class XMapX[A, B](forward: Map[A, B] = Map[A, B](), reverse: Map[B, A] = Map[B, A]()) {
      def get(a: A): Option[B] = forward.get(a)

      def addNew(e: (A, B)): Option[XMapX[A, B]] = {
        if (forward.contains(e._1) || reverse.contains(e._2)) None
        else Some(copy(forward + e, reverse + e.swap))
      }
    }

    /** mapping from unique ids in a to elements in b */
    case class Subst(
      calls: XMapX[CallId, CallId],
      invocs: XMapX[InvocationId, InvocationId],
      txns: XMapX[TransactionId, TransactionId],
      domainValues: XMapX[DomainValue, DomainValue]
    ) {

      def |>(function: Subst => LazyList[Subst]): LazyList[Subst] = function(this)

    }

    trait Match[T] {
      def findMatches(a: T, b: T, s: Subst): LazyList[Subst]

    }

    def m[T](a: T, b: T)(implicit ma: Match[T]): Subst => LazyList[Subst] =
      s => ma.findMatches(a, b, s)

    def mEq[T]: Match[T] = (a, b, s) => if (a == b) LazyList(s) else LazyList()

    implicit def mMap[K, V](implicit mk: Match[K], mv: Match[V]): Match[Map[K, V]] = { (a, b, s) =>

      matchUnordered(a.toSet, b.toSet, s)
    }

    implicit def mPair[K, V](implicit mk: Match[K], mv: Match[V]): Match[(K, V)] = { (a, b, s) =>
      s |> m(a._1, b._1) >> m(a._2, b._2)
    }

    implicit def mOption[T](implicit mt: Match[T]): Match[Option[T]] = { (a, b, s) =>
      (a, b) match {
        case (None, None) => LazyList(s)
        case (Some(x), Some(y)) => s |> m(x, y)
        case _ => LazyList()
      }
    }

    implicit def mList[T](implicit mt: Match[T]): Match[List[T]] = { (a, b, s) =>
      matchOrdered(a, b, s)
    }

    implicit def mSet[T](implicit mt: Match[T]): Match[Set[T]] = { (a, b, s) =>
      matchUnordered(a, b, s)
    }

    def matchOrdered[T](a: Iterable[T], b: Iterable[T], s: Subst)(implicit mt: Match[T]): LazyList[Subst] = {
      if (a.isEmpty && b.isEmpty)
        LazyList(s)
      else if (a.nonEmpty && b.nonEmpty) {
        for {
          s2 <- s |> m(a.head, b.head)
          s3 <- matchOrdered(a.tail, b.tail, s2)
        } yield s3
      } else {
        // one empty, other nonempty
        LazyList()
      }
    }

    def matchUnordered[T](a: Set[T], b: Set[T], s: Subst)(implicit mt: Match[T]): LazyList[Subst] = {
      if (a.isEmpty && b.isEmpty)
        LazyList(s)
      else if (a.nonEmpty && b.nonEmpty) {
        val first = a.head
        val aRest = a.tail
        for {
          x <- b.to(LazyList)
          bRest = b - x
          s2 <- m(first, x)(mt)(s)
          s3 <- matchUnordered(aRest, bRest, s2)
        } yield s3
      } else {
        // one empty, other nonempty -> cannot match
        LazyList()
      }
    }

    implicit def mInt: Match[Int] = mEq

    implicit def mBoolean: Match[Boolean] = mEq

    implicit def mString: Match[String] = mEq

    implicit def mIdType: Match[IdType] = mEq

    implicit def mLocalVar: Match[Interpreter.LocalVar] = mEq

    implicit def mAnyValue: Match[AnyValue] = { (a, b, s) =>
      (a.value, b.value) match {
        case (d1: DomainValue, d2: DomainValue) =>
          s |> m(d1, d2)
        case (x, y) =>
          mEq.findMatches(x, y, s)
      }
    }

    implicit def mDomainValue: Match[DomainValue] = { (a, b, s) =>
      if (a.name == b.name)
        s.domainValues.get(a) match {
          case Some(a2) =>
            mEq.findMatches(a2, b, s)
          case None =>
            s.domainValues.addNew(a -> b).map(x => s.copy(domainValues = x)).to(LazyList)
        }
      else
        LazyList()
    }

    implicit def mCallId: Match[CallId] = { (a, b, s) =>
      s.calls.get(a) match {
        case Some(a2) =>
          mEq.findMatches(a2, b, s)
        case None =>
          s.calls.addNew(a -> b).map(x => s.copy(calls = x)).to(LazyList)
      }
    }

    implicit def mInvocationId: Match[InvocationId] = { (a, b, s) =>
      s.invocs.get(a) match {
        case Some(a2) =>
          mEq.findMatches(a2, b, s)
        case None =>
          s.invocs.addNew(a -> b).map(x => s.copy(invocs = x)).to(LazyList)
      }
    }

    implicit def mTransactionId: Match[TransactionId] = { (a, b, s) =>
      s.txns.get(a) match {
        case Some(a2) =>
          mEq.findMatches(a2, b, s)
        case None =>
          s.txns.addNew(a -> b).map(x => s.copy(txns = x)).to(LazyList)
      }
    }


    implicit def mState: Match[State] = { (a, b, s) =>
      //      val invocsA = a.invocations.groupBy(_._2.operation.operationName).view.mapValues(_.values.toSet).toMap
      //      val invocsB = b.invocations.groupBy(_._2.operation.operationName).view.mapValues(_.values.toSet).toMap
      //
      //      if (invocsA.keySet != invocsB.keySet)
      //        LazyList()
      //      else {
      //        for {
      //          (op, invocsOpA) <- invocsA.to(LazyList)
      //          invocsOpB = invocsB(op)
      //          s2 <- matchUnordered(invocsOpA.map(x => (x.id, x.operation)),
      //            invocsOpB.map(x => (x.id, x.operation)), s)
      //          // next match calls in the invocation
      //          x = a.localStates()
      //        } yield {
      //
      //        }
      s |>
        m(a.maxCallId, b.maxCallId) >>
          m(a.maxTransactionId, b.maxTransactionId) >>
          m(a.maxInvocationId, b.maxInvocationId) >>
          m(a.invocations, b.invocations) >>
          m(a.localStates, b.localStates) >>
          m(a.transactions, b.transactions) >>
          m(a.calls, b.calls) >>
          m(a.knownIds, b.knownIds) >>
          m(a.generatedIds, b.generatedIds)

    }


    implicit def mCallInfo: Match[CallInfo] = { (a, b, s) =>
      s |>
        m(a.origin, b.origin) >>
          m(a.callTransaction, b.callTransaction) >>
          m(a.operation, b.operation) >>
          m(a.id, b.id) >>
          m(a.callClock, b.callClock)
    }

    implicit def mInvocationInfo: Match[InvocationInfo] = { (a, b, s) =>
      s |>
        m(a.operation, b.operation) >>
          m(a.result, b.result) >>
          m(a.id, b.id)
    }

    implicit def mTransactionInfo: Match[TransactionInfo] = { (a, b, s) =>
      s |>
        m(a.origin, b.origin) >>
          m(a.finished, b.finished) >>
          m(a.currentCalls, b.currentCalls) >>
          m(a.start, b.start) >>
          m(a.id, b.id)
    }

    implicit def mLocalState: Match[LocalState] = { (a, b, s) =>
      s |>
        m(a.currentInvoc, b.currentInvoc) >>
          m(a.varValues, b.varValues) >>
          m(a.todo, b.todo) >>
          m(a.waitingFor, b.waitingFor) >>
          m(a.currentTransaction, b.currentTransaction) >>
          m(a.visibleCalls, b.visibleCalls)
    }


    implicit def mStatementOrAction: Match[StatementOrAction] = mEq

    implicit def mLocalWaitingFor: Match[LocalWaitingFor] = mEq

    implicit def mDataTypeValue: Match[DataTypeValue] = { (a, b, s) =>
      s |>
        m(a.operationName, b.operationName) >>
          m(a.args, b.args)
    }

    implicit def mSnapshotTime: Match[SnapshotTime] = { (a, b, s) =>
      s |> m(a.snapshot, b.snapshot)
    }


    implicit class Combinators(f: Subst => LazyList[Subst]) {
      def >>(g: Subst => LazyList[Subst]): Subst => LazyList[Subst] =
        s => f(s).flatMap(s2 => g(s2))

    }

  }

  private val initialState = State(interpreter = Some(interpreter))


  def randomTestsSingle(limit: Int, debug: Boolean = true, sequentialMode: Boolean = true): Option[QuickcheckCounterexample] = {

    val states: mutable.MultiDict[Int, HashState] = mutable.MultiDict.empty
    val workList: mutable.Queue[HashState] = mutable.Queue.empty


    workList.enqueue(HashState.fromState(initialState, Trace(List())))

    def enqueStateIfNew(state: State, actions: Trace, i: Int): Unit = {
      val hash = HashState.hashState(state)

      val sameHash = states.get(hash)
      if (sameHash.nonEmpty)
        println(s"possible Equivalent: ${sameHash.size}")
      for (oldState <- sameHash) {
        println("compare")
        println("  " + state.invocations.toList.sortBy(_._1.id).map(_._2.operation).mkString(", "))
        println("  " + oldState.getState.invocations.toList.sortBy(_._1.id).map(_._2.operation).mkString(", "))
        if (StateEq.statesEquivalent(state, oldState.getState)) {
          return
        }
      }
      val hs = new HashState(hash, actions.add(i), state)
      states += hash -> hs
      workList.enqueue(hs)
    }

    var i = 0

    while (workList.nonEmpty) {
      val hs = workList.dequeue()
      val s = hs.getState

      //      println("Working on state: ")
      //      println(s.invocations.values.map(_.operation).mkString(", "))
      i += 1
      println(s"$i. worklist size: ${workList.size} ")
      println(s.invocations.toList.sortBy(_._1.id).map(_._2.operation).mkString(", "))

      for ((a, i) <- possibleActions(s, true).zipWithIndex) {
        if (Thread.currentThread().isInterrupted) {
          return None
        }
        executeAction(s, a) match {
          case Some(newS) =>
            if (newS.ive.isDefined) {
              return Some(makeCounterExample(newS))
            }
            enqueStateIfNew(newS.state, hs.actions, i)

          case None =>
        }
      }
    }
    println("Checked all states")
    None
  }

  private def makeCounterExample(s: S): QuickcheckCounterexample = {
    val e = s.ive.get
    val renderResult = Visualization.renderStateGraph(prog, e.state)

    QuickcheckCounterexample(
      brokenInvariant = e.inv.source.range,
      info = e.info,
      state = e.state,
      trace = printTrace(s.trace),
      counterExampleRender = renderResult
    )
  }


}
