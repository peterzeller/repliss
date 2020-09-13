package crdtver.testing

import java.time.Duration
import java.util.Objects

import crdtver.Repliss.QuickcheckCounterexample
import crdtver.RunArgs
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionType, IdType, InProgram, InTypeExpr, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType, _}
import crdtver.testing.Interpreter._
import crdtver.testing.SmallcheckTester2.StateEq
import crdtver.utils.DebugPrint.debugPrint
import crdtver.utils.DurationUtils.{DurationExt, DurationUnits}
import crdtver.utils.{DebugPrint, LazyListUtils, TimeTaker}
import crdtver.utils.LazyListUtils.{Lazy, LazyListExtensions}

import scala.collection.immutable.{::, Nil}
import scala.collection.mutable
import scala.ref.WeakReference
import scala.util.hashing.MurmurHash3

/**
 * This class is responsible for executing random tests on a Repliss program.
 */
class SmallcheckTester2(prog: InProgram, runArgs: RunArgs) {


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


  private def possibleActions(state: State, sequentialMode: Boolean)(implicit ctxt: Ctxt): LazyList[Action] = {

    val localActions: scala.LazyList[Action] = possibleLocalActions(state)
    //    val invChecks: LazyList[Action] = newRandomInvariantCheck(state)

    val invocations: LazyList[Action] =
      if (sequentialMode && localActions.nonEmpty)
      // first finish local actions before starting new invocations
        LazyList()
      else
        newRandomInvocation(state)

    localActions ++ invocations
  }


  private def possibleLocalActions(state: State)(implicit ctxt: Ctxt): LazyList[Action] = {
    val waitFors: LazyList[(InvocationId, LocalWaitingFor)] = getLocalWaitingFors(state)
    val localActions: LazyList[Action] =
      for {
        (invoc, waitingFor) <- waitFors
        // only generate maxUsedIds
        if (waitingFor match {
          case WaitForNewId(_, typename) =>
            state.generatedIds.getOrElse(typename, Set()).size < ctxt.maxUsedIds
          case _ =>
            true
        })
        a <- makeAction(state, invoc, waitingFor)
      } yield a
    localActions
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


  private def randomValue(typ: InTypeExpr, state: State)(implicit ctxt: Ctxt): LazyList[AnyValue] = {
    ctxt.interpreter.enumerateValues(typ, state)
  }

  private def newRandomInvocation(state: State)(implicit ctxt: Ctxt): LazyList[Action] = {
    if (state.invocations.size > ctxt.maxInvocations) {
      // reached maximum list of invocations
      return LazyList()
    }
    val invocId = InvocationId(state.maxInvocationId + 1)
    // sort procs to prefer the ones that have not been called yet
    val procs = prog.procedures.sortBy(p =>
      state.invocations.values.count(i => i.operation.operationName == p.name.name))
    for {
      proc <- procs.to(LazyList).breadthFirst
      args <- LazyListUtils.allCombinations(proc.params.map(param => randomValue(param.typ, state)))
    } yield CallAction(invocId, proc.name.name, args)
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


  class HashState(
    val hash: Int,
    state: State,
    actions: List[Action],
    parent: Option[HashState]
  ) {

    def recoverTrace: List[Action] =
      parent.map(_.recoverTrace).getOrElse(List()) ++ actions

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
    def fromState(s: State, actions: List[Action], parent: Option[HashState]): HashState = {
      new HashState(hashState(s), s, actions, parent)
    }

    def hashState(s: State): Int = hash(s).combine

    def hash[T](t: T)(implicit h: Hashable[T]): HashRes =
      h.hash(t)

    case class HashRes(
      hash: Int,
      counts: Map[Any, Int] = Map()
    ) {
      def combine: Int = combineHashs(hash, counts.values.toList.sorted.hashCode())

      def +(other: HashRes): HashRes =
        combineHashs(this, other)
    }

    trait Hashable[T] {
      def hash(t: T): HashRes
    }

    implicit def hashState: Hashable[State] =
      s => combineHashs(List(
        hash(s.calls),
        HashRes(s.maxCallId),
        hash(s.transactions),
        HashRes(s.maxTransactionId),
        hash(s.invocations),
        HashRes(s.maxInvocationId),
        hash(s.knownIds),
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
        Iterable(HashRes(981923), HashRes(m.size)) ++ m.iterator.map(hash(_))
      )

    implicit def hashOption[T](implicit h: Hashable[T]): Hashable[Option[T]] =
      m => combineHashs(
        Iterable(HashRes(if (m.isDefined) 96824 else 42356)) ++ m.iterator.map(hash(_))
      )

    implicit def hashCallId: Hashable[CallId] = c => HashRes(0, Map(c -> 1))

    implicit def hashInvocationId: Hashable[InvocationId] = c => HashRes(0, Map(c -> 1))

    implicit def hashTransactionId: Hashable[TransactionId] = c => HashRes(0, Map(c -> 1))

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

    implicit def hashStatementOrAction: Hashable[StatementOrAction] = c => HashRes(c.hashCode())


    implicit def hashLocalWaitingFor: Hashable[LocalWaitingFor] = {
      case w@WaitForBegin() => HashRes(w.hashCode())
      case w@WaitForNothing() => HashRes(w.hashCode())
      case w@WaitForBeginTransaction() => HashRes(w.hashCode())
      case WaitForFinishInvocation(result) => hash(result)
      case w@(id: WaitForNewId) => HashRes(w.hashCode())
    }

    implicit def hashIdType: Hashable[IdType] = c => HashRes(c.hashCode())

    implicit def hashLocalVar: Hashable[Interpreter.LocalVar] = c => HashRes(c.hashCode())

    implicit def hashSnapshotTime: Hashable[SnapshotTime] = { v =>
      HashRes(51, v.snapshot.map(x => x -> 1).toMap)
    }

    implicit def hashDataTypeValue: Hashable[DataTypeValue] = { v =>
      combineHashs(
        Iterable(hash(v.operationName)) ++ v.args.map(hash(_))
      )
    }

    implicit def hashString: Hashable[String] = c => HashRes(c.hashCode)

    implicit def hashBoolean: Hashable[Boolean] = c => HashRes(c.hashCode)

    implicit def hashAnyValue: Hashable[AnyValue] = { av =>
      av.value match {
        case d: DataTypeValue => hash(d)
        case d: DomainValue =>
          hash(d.name)
        case x: String =>
          HashRes(x.hashCode())
        case x: Boolean =>
          HashRes(x.hashCode())
        case x =>
          println(s"unhandled AnyValue $x (${x.getClass})")
          HashRes(0)
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

    def combineHashs(l: HashRes*): HashRes = combineHashs(l)


    def combineCounts(a: Map[Any, Int], b: Map[Any, Int]): Map[Any, Int] = {
      (a.toSeq ++ b).groupMapReduce(_._1)(_._2)(_ + _)
    }

    def combineHashs(l: IterableOnce[HashRes]): HashRes = {
      var result = 1
      var counts = Map[Any, Int]()
      l.iterator.foreach { element =>
        result = 31 * result + element.combine
        counts = combineCounts(counts, element.counts)
      }
      HashRes(result, counts)
    }

    def unorderedHash(seed: Int, xs: HashRes*): HashRes =
      unorderedHash(seed, xs)

    def unorderedHash(seed: Int, xs: IterableOnce[HashRes]): HashRes = {
      var a, b, n = 0
      var c = 1
      var counts = Map[Any, Int]()
      xs.iterator foreach { x =>
        counts = combineCounts(counts, x.counts)
        val h = x.combine
        a += h
        b ^= h
        c *= h | 1
        n += 1
      }
      var h = seed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mix(h, b)
      h = MurmurHash3.mixLast(h, c)
      h = MurmurHash3.finalizeHash(h, n)
      HashRes(h, counts)

    }

  }


  case class S(state: State, trace: List[Action], ive: Option[InvariantViolationException] = None) {
    def prependTrace(trace: List[Action]): S =
      copy(trace = trace ++ this.trace)

  }


  private def executeAction(state: State, action: Action)(implicit ctxt: Ctxt): Option[S] = {
    try {
      ctxt.interpreter.executeAction(state, action) match {
        case Some(newState) =>
          try {
            ctxt.interpreter.checkInvariants(newState)
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


  private val initialState = State()


  def executeActionAndLocals(s: State, a: Action)(implicit ctxt: Ctxt): LazyList[S] = {
    executeAction(s, a) match {
      case Some(newS) =>
        newS.ive match {
          case Some(exc) =>
            LazyList(newS)
          case None =>
            val next = possibleLocalActions(newS.state)
            if (next.isEmpty)
              LazyList(newS)
            else
              next.flatMap(a => executeActionAndLocals(newS.state, a).map(_.prependTrace(newS.trace)))
        }
      case None =>
        LazyList()
    }
  }

  case class Ctxt(
    // custom data types can have values 0 <= x < domainSize
    domainSize: Int,
    // maximum number of known ids for generating random values
    maxUsedIds: Int,
    maxInvocations: Int
  ) {
    val interpreter = new Interpreter(prog, runArgs, domainSize)
  }

  def randomTestsSingle(limit: Int, debug: Boolean = true, sequentialMode: Boolean = true): Option[QuickcheckCounterexample] = {

    val states: mutable.MultiDict[Int, HashState] = mutable.MultiDict.empty
    val workList: mutable.Queue[(HashState, LazyList[Action])] = mutable.Queue.empty

    // context includes the current bounds of the state exploration
    var ctxt = Ctxt(
      domainSize = 1,
      maxUsedIds = 1,
      maxInvocations = 1
    )





    def enqueStateIfNew(state: State, actions: List[Action], parent: HashState): Unit = {
      val hash = HashState.hashState(state)

      val sameHash = states.get(hash)
      for (oldState <- sameHash) {
        if (StateEq.statesEquivalent(state, oldState.getState)) {
          return
        }
      }
      val hs = new HashState(hash, state, actions, Some(parent))
      states += hash -> hs
      val newActions = possibleActions(state, true)(ctxt)
      workList.enqueue((hs, newActions))
    }

    for (i <- LazyList.from(1)) {
      assert(workList.isEmpty)
      // enqueue initial state
      workList.enqueue((HashState.fromState(initialState, List(), None), possibleActions(initialState, true)(ctxt)))
      while (workList.nonEmpty) {
        val (hs, actions) = workList.dequeue()
        if (actions.nonEmpty) {
//          println(s"${hs.recoverTrace.filter(_.isInstanceOf[CallAction])}")
          val a = actions.head
          val s = hs.getState

          for {
            newS <- executeActionAndLocals(s, a)(ctxt)
          } {
            if (Thread.currentThread().isInterrupted) {
              return None
            }

            if (newS.ive.isDefined) {
              return Some(makeCounterExample(newS.prependTrace(hs.recoverTrace)))
            }
            enqueStateIfNew(newS.state, newS.trace, hs)
          }
          workList.enqueue((hs, actions.tail))
        }
      }
      println(s"Finished depth $i")
      // deepen ctxt
      ctxt = Ctxt(
        maxInvocations = i,
        domainSize = 1 + i / 4,
        maxUsedIds = 1 + i / 4
      )
    }

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

object SmallcheckTester2 {


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

      matchUnorderedMap(a, b, s)
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
        val aRest = Lazy(a.tail)
        for {
          x <- b.to(LazyList)
          bRest = Lazy(b - x)
          s2 <- m(first, x)(mt)(s)
          s3 <- matchUnordered(aRest.get, bRest.get, s2)
        } yield s3
      } else {
        // one empty, other nonempty -> cannot match
        LazyList()
      }
    }

    def matchUnorderedMap[T, V](a: Map[T, V], b: Map[T, V], s: Subst)(implicit mt: Match[T], mv: Match[V]): LazyList[Subst] = {
      if (a.isEmpty && b.isEmpty)
        LazyList(s)
      else if (a.nonEmpty && b.nonEmpty) {
        val first = a.head
        val aRest = Lazy(a.tail)
        for {
          x <- b.to(LazyList)
          bRest = Lazy(b - x._1)
          s2 <- m(first._2, x._2)(mv)(s)
          s3 <- m(first._1, x._1)(mt)(s2)
          s4 <- matchUnorderedMap(aRest.get, bRest.get, s3)
        } yield s4
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

    implicit def orderingCallInfo: Ordering[CallInfo] = { (x: CallInfo, y: CallInfo) =>
      if (y.callClock.snapshot.contains(x.id))
        -1
      else if (x.callClock.snapshot.contains(y.id))
        1
      else 0
    }


    implicit def mState: Match[State] = { (a, b, s) =>
      val invocsA = a.invocations.groupBy(_._2.operation.operationName).view.mapValues(_.values.toSet).toMap
      val invocsB = b.invocations.groupBy(_._2.operation.operationName).view.mapValues(_.values.toSet).toMap

      if (invocsA.keySet != invocsB.keySet)
        LazyList[Subst]()
      else {


        def mInvocs(s: Subst): LazyList[Subst] = {

          def rec(ops: List[String], s: Subst): LazyList[Subst] = ops match {
            case List() => LazyList(s)
            case op :: opsRest =>
              val invocsOpA = invocsA(op)
              val invocsOpB = invocsB(op)
              for {
                s2 <- matchUnordered(
                  invocsOpA.map(x => (x.id, x.operation)),
                  invocsOpB.map(x => (x.id, x.operation)), s)
                s3 <- rec(opsRest, s2)
              } yield s3
          }

          rec(invocsA.keys.toList, s)
        }

        def mCalls(s: Subst): LazyList[Subst] = {

          def rec(invocs: List[InvocationId], s: Subst): LazyList[Subst] = invocs match {
            case List() => LazyList(s)
            case aId :: invocsRest =>
              val bId = s.invocs.get(aId).getOrElse(throw new Exception(s"$aId not matched yet"))
              val aCalls = a.calls.values.toList.filter(_.origin == aId).sorted
              val bCalls = b.calls.values.toList.filter(_.origin == bId).sorted
              for {
                s2 <- matchOrdered(aCalls, bCalls, s)(mCallInfoPreFilter)
                s3 <- rec(invocsRest, s2)
              } yield {
                s3
              }
          }

          rec(a.invocations.keys.toList, s)
        }

        s |>
          m(a.maxCallId, b.maxCallId) >>
            m(a.maxTransactionId, b.maxTransactionId) >>
            m(a.maxInvocationId, b.maxInvocationId) >>
            mInvocs >>
            mCalls >>
            m(a.invocations, b.invocations) >>
            m(a.localStates, b.localStates) >>
            m(a.transactions, b.transactions) >>
            m(a.calls, b.calls) >>
            m(a.knownIds, b.knownIds) >>
            m(a.generatedIds, b.generatedIds)
      }
    }


    implicit def mCallInfo: Match[CallInfo] = { (a, b, s) =>
      s |>
        m(a.origin, b.origin) >>
          m(a.callTransaction, b.callTransaction) >>
          m(a.operation, b.operation) >>
          m(a.id, b.id) >>
          m(a.callClock, b.callClock)
    }

    /** same as mCallInfo but without matching callClock */
    def mCallInfoPreFilter: Match[CallInfo] = { (a, b, s) =>
      s |>
        m(a.origin, b.origin) >>
          m(a.callTransaction, b.callTransaction) >>
          m(a.operation, b.operation) >>
          m(a.id, b.id)
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
      if (a.snapshot.size != b.snapshot.size)
        LazyList()
      else {
        val matched: Set[(CallId, CallId)] = a.snapshot.flatMap(c => s.calls.get(c).map(c2 => (c, c2)))
        val matchedA = matched.map(_._1)
        val matchedB = matched.map(_._2)
        if (!matchedB.subsetOf(b.snapshot))
          LazyList()
        else if (matchedA.size == a.snapshot.size)
          LazyList(s)
        else {
          val unmatchedA = a.snapshot -- matchedA
          val unmatchedB = b.snapshot -- matchedB
          s |> m(unmatchedA, unmatchedB)
        }
      }
    }


    implicit class Combinators(f: Subst => LazyList[Subst]) {
      def >>(g: Subst => LazyList[Subst]): Subst => LazyList[Subst] =
        s => f(s).flatMap(s2 => g(s2))

    }

  }


}