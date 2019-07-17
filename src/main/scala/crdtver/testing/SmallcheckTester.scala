package crdtver.testing

import java.util.concurrent.atomic.AtomicBoolean

import crdtver.Repliss.QuickcheckCounterexample
import crdtver.RunArgs
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionType, IdType, InProgram, InTypeExpr, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType, _}
import crdtver.testing.Interpreter._
import crdtver.utils.LazyListUtils.LazyListExtensions
import crdtver.utils.StreamUtils.StreamExtensions

import scala.collection.immutable.{::, Nil}
import scala.util.Random

/**
  * This class is responsible for executing random tests on a Repliss program.
  */
class SmallcheckTester(prog: InProgram, runArgs: RunArgs) {

  // custom data types can have values 0 <= x < domainSize
  val domainSize = 3

  // maximum number of known ids for generating random values
  val maxUsedIds = 2

  val interpreter = new Interpreter(prog, runArgs, domainSize)


  val debug = false

  def debugLog(s: String): Unit = {
    if (debug) {
      println(s)
    }
  }

  sealed abstract class ActionProvider {
    def discardLast(): Unit

    def nextAction(state: State): Option[Action]
  }

  class TraceActionProvider(trace: List[Action]) extends ActionProvider {
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


  private def newRandomInvariantCheck(state: State): LazyList[Action] = {
    import LazyList._

    val keys: LazyList[InvocationId] = state.localStates.keys.to[LazyList]
    keys.map(id => InvariantCheck(id))
  }

  private def possibleActions(state: State): LazyList[Action] = {

    val waitFors: LazyList[(InvocationId, LocalWaitingFor)] = getLocalWaitingFors(state)
    val localActions: LazyList[Action] =
      for ((invoc, waitingFor) <- waitFors; a <- makeAction(state, invoc, waitingFor)) yield a

    val invChecks: LazyList[Action] = newRandomInvariantCheck(state)

    val invocations: LazyList[Action] = newRandomInvoaction(state)

    localActions ++ invChecks ++ invocations
  }


  private def getPulledTransactions2(state: State, invoc: InvocationId): LazyList[Set[TransactionId]] = {
    val allTransactions = state.transactions.keySet
    val ls: LocalState = state.localStates.getOrElse(invoc, return LazyList(Set()))


    // get transactions, which are not yet in current snapshot
    val pullableTransactions = allTransactions.filter(tx => !ls.isTransactionVisible(tx, state))

    def happensBefore(t1: TransactionId, t2: TransactionId): Boolean = {
      val a = state.transactions(t1)
      val b = state.transactions(t2)
      a.happenedBefore(b)
    }

    allDownwardsClosedSubsets(pullableTransactions, happensBefore)
  }

  private def makeAction(state: State, invoc: InvocationId, waitingFor: LocalWaitingFor): LazyList[Action] = waitingFor match {
    case WaitForBeginTransaction() =>
      for (pulledTransactions: Set[TransactionId] <- getPulledTransactions2(state, invoc)) yield
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
      case SimpleType(name) =>
        // TODO handle datatypes
        for (i <- (0 until domainSize).to[LazyList]) yield
          Interpreter.domainValue(name, i)
      case idt@IdType(_name) =>
        // TODO should include generatedIds
        knownIds.get(idt) match {
          case Some(s) =>
            // only pick from the first N (maxUsedIds) unique identifiers to make it more likely that we work on the same data:
            s.keys.to[LazyList].take(maxUsedIds)
          case None =>
            LazyList()
        }
      case BoolType() =>
        LazyList(false, true).map(AnyValue)
      case IntType() =>
        for (i <- (0 until 100).to[LazyList]) yield
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
    }
  }

  private def newRandomInvoaction(state: State): LazyList[Action] = {
    val invocId = InvocationId(state.maxInvocationId + 1)
    for {
      proc <- new LazyListExtensions(prog.procedures.to[LazyList]).breadthFirst
      args <- for (param <- proc.params.to[LazyList]) yield
        for (v <- randomValue(param.typ, state.knownIds)) yield v

    } yield CallAction(invocId, proc.name.name, args.toList)
  }


  private def allDownwardsClosedSubsets[T](set: Set[T], lessThan: (T, T) => Boolean)(implicit ordering: Ordering[T]): LazyList[Set[T]] = {
    if (set.isEmpty)
      return LazyList(Set())
    // take minimal elements
    val m = minimalElements(set, lessThan)
    val remaining1 = set -- m
    for {
      // all subsets of minimal elements
      ms <- allSubsets(m)
      // let remaining2 be the non-minimal elements that still have all dependencies from m in ms
      other = m -- ms
      remaining2 = remaining1.filter(x => !other.exists(y => lessThan(y, x)))
      // recursively get all combinations for the rest
      rs <- allDownwardsClosedSubsets(remaining2, lessThan)
    } yield ms ++ rs
  }

  private def minimalElements[T](set: Set[T], lessThan: (T, T) => Boolean): Set[T] = {
    set.filter(x => !set.exists(y => lessThan(x, y)))
  }

  private def allSubsets[T](set: Set[T]): LazyList[Set[T]] = {
    allSublists(set.toList).map(_.toSet)
  }

  private def allSublists[T](list: List[T]): LazyList[List[T]] = list match {
    case Nil =>
      LazyList(List())
    case x :: xs =>
      for (sl <- allSublists(xs); y <- LazyList(sl, x :: sl)) yield y
  }


  private def getLocalWaitingFors(state: State): LazyList[(InvocationId, LocalWaitingFor)] = {
    val result = for ((invocation, ls) <- state.localStates) yield {
      (invocation, ls.waitingFor)
    }
    result.to[LazyList]
  }


  def pickRandom[T](list: List[T])(implicit rand: Random): T = {
    if (list.isEmpty)
      throw new IllegalArgumentException("List empty")
    val i = rand.nextInt(list.size)
    list(i)
  }


  def printTrace(trace: List[Action]): String = {
    val sb = new StringBuilder
    for (action <- trace) {
      sb.append(s"$action\n")
    }
    sb.toString()
  }


  def randomTestsSingle(limit: Int, seed: Int = 0, debug: Boolean = true, cancellationToken: AtomicBoolean): Option[QuickcheckCounterexample] = {
    var startTime = System.nanoTime()

    val interpreter = new Interpreter(prog, RunArgs(), domainSize = 3)


    case class S(state: State, reverseTrace: List[Action], ive: Option[InvariantViolationException] = None)

    val initialState = S(State(interpreter = Some(interpreter)), List())

    def executeAction(s: S, action: Action): Option[S] = {
      try {
        interpreter.executeAction(s.state, action) match {
          case Some(newState) =>
            Some(S(newState, action::s.reverseTrace, None))
          case None =>
            None
        }
      } catch {
        case ive: InvariantViolationException =>
          Some(s.copy(ive = Some(ive)))
      }

    }

    def children(s: S): LazyList[S] = {
      if (s.ive.isDefined)
        LazyList()
      else
        for {
          action <- possibleActions(s.state)
          newState <- LazyList.fromTraversable(executeAction(s, action))
        } yield newState
    }

    TreeWalker.walkTree2[S](initialState, breadth = 2, children = children)
      .takeWhile(_ => !cancellationToken.get())
      .find((s: S) => s.ive.isDefined)
      .map((s: S) => {
        val e = s.ive.get
        val (dot, svg) = Visualization.renderStateGraph(prog, e.state)

        QuickcheckCounterexample(
          brokenInvariant = e.inv.source.range,
          info = e.info,
          state = e.state,
          trace = printTrace(s.reverseTrace.reverse),
          counterExampleSvg = svg,
          counterExampleDot = dot
        )
      })

  }


  def removeAtIndex[T](l: List[T], index: Int): List[T] =
    l.take(index) ++ l.drop(index + 1)


  def execute(actionProvider: ActionProvider): State = {
    var state = State(interpreter = Some(this.interpreter))
    var i = 0
    while (true) {
      i += 1
      val action = actionProvider.nextAction(state)
      debugLog(s"action = $action")
      if (action.isEmpty) {
        return state
      }
      interpreter.executeAction(state, action.get) match {
        case Some(s) =>
          state = s
        case None =>
          actionProvider.discardLast()
      }
    }
    return state
  }


}