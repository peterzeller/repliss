package crdtver.testing

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.{Callable, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import crdtver.{Repliss, RunArgs}
import crdtver.Repliss.QuickcheckCounterexample
import crdtver.language.AtomicTransform
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionType, IdType, InProgram, InTypeExpr, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType}
import crdtver.testing.Interpreter._
import crdtver.language.TypedAst._
import crdtver.utils.{ConcurrencyUtils, Helper}

import scala.collection.immutable.{::, Nil}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Random, Success}
import scala.concurrent.duration._

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

     def getTrace(): List[Action] = exeutedActions.reverse

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

  class RandomActionProvider(limit: Int, seed: Int = 0, cancellationToken: AtomicBoolean) extends ActionProvider {
    private var maxGivenId = 0

    def newRandomInvariantCheck(state: State): Stream[Action] = {
      for (id <- state.localStates.keys.toStream) yield InvariantCheck(id)
    }

    def possibleActions(state: State): Stream[Action] = {

      val waitFors: Stream[(InvocationId, LocalWaitingFor)] = getLocalWaitingFors(state)
      val localActions: Stream[Action] =
        for ((invoc, waitingFor) <- waitFors; a <- makeAction(state, invoc, waitingFor)) yield a

      val invChecks: Stream[Action] = newRandomInvariantCheck(state)

      val invocations: Stream[Action] = newRandomInvoaction(state)

      localActions ++ invChecks ++ invocations
    }


    def getPulledTransactions2(state: State, invoc: InvocationId): Stream[Set[TransactionId]] = {
      val allTransactions = state.transactions.keySet
      val ls: LocalState = state.localStates.getOrElse(invoc, return Stream(Set()))


      // get transactions, which are not yet in current snapshot
      val pullableTransactions = allTransactions.filter(tx => !ls.isTransactionVisible(tx, state))
      def happensBefore(t1: TransactionId, t2: TransactionId): Boolean = {
        val a = state.transactions(t1)
        val b = state.transactions(t2)
        a.happenedBefore(b)
      }

      allDownwardsClosedSubsets(pullableTransactions, happensBefore)
    }

    def makeAction(state: State, invoc: InvocationId, waitingFor: LocalWaitingFor): Stream[Action] = waitingFor match {
      case WaitForBeginTransaction() =>
        for (pulledTransactions: Set[TransactionId] <- getPulledTransactions2(state, invoc)) yield
          LocalAction(invoc,
            StartTransaction(
              newTransactionId = TransactionId(state.maxTransactionId + 1),
              pulledTransaction = pulledTransactions
            )
          )
      case WaitForFinishInvocation(_) =>
        Stream(LocalAction(invoc, Return()))
      case WaitForNewId(_, _) =>
        maxGivenId += 1
        Stream(LocalAction(invoc, NewId(maxGivenId)))
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

    def randomValue(typ: InTypeExpr, knownIds: Map[IdType, Map[AnyValue, InvocationId]]): Stream[AnyValue] = {
      typ match {
        case SimpleType(name) =>
          // TODO handle datatypes
          for (i <- (0 until domainSize).toStream) yield
            Interpreter.domainValue(name, i)
        case idt@IdType(_name) =>
          // TODO should include generatedIds
          knownIds.get(idt) match {
            case Some(s) =>
              // only pick from the first N (maxUsedIds) unique identifiers to make it more likely that we work on the same data:
              s.keys.toStream.take(maxUsedIds)
            case None =>
              Stream()
          }
        case BoolType() =>
          Stream(false, true).map(AnyValue)
        case IntType() =>
          for (i <- (0 until 100).toStream) yield
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

    def newRandomInvoaction(state: State): Stream[Action] = {
      val invocId = InvocationId(state.maxInvocationId + 1)
      var tries = 0
      while (tries < 1000) {
        tries += 1
        var procedures = prog.procedures
        // TODO handle several id types
        //        if (state.knownIds.size >= maxUsedIds) {
        //          // don't call more functions returning id-types
        //          procedures = procedures.filter(p => p.returnType match {
        //            case Some(IdType(name, source)) =>
        //              false
        //            case _ => true
        //          })
        //        }

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

    def allDownwardsClosedSubsets[T](set: Set[T], lessThan: (T, T) => Boolean)(implicit ordering: Ordering[T]): Stream[Set[T]] = {
      if (set.isEmpty)
        return Stream(Set())
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

    def minimalElements[T](set: Set[T], lessThan: (T, T) => Boolean): Set[T] = {
      set.filter(x => !set.exists(y => lessThan(x, y)))
    }

    def allSubsets[T](set: Set[T]): Stream[Set[T]] = {
      allSublists(set.toList).map(_.toSet)
    }

    def allSublists[T](list: List[T]): Stream[List[T]] = list match {
      case Nil =>
        Stream(List())
      case x::xs =>
        for (sl <- allSublists(xs); y <- Stream(sl, x::sl)) yield y
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

    private def getLocalWaitingFors(state: State): Stream[(InvocationId, LocalWaitingFor)] = {
      val result = for ((invocation, ls) <- state.localStates) yield {
        (invocation, ls.waitingFor)
      }
      result.toStream
    }

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

  def randomTests(limit: Int, threads: Int, seed: Int = 0, debug: Boolean = false, timeLimit: Duration = 2.minutes): Option[QuickcheckCounterexample] = {

    import ExecutionContext.Implicits.global

    val cancellationToken = new AtomicBoolean(false)
    try {
      val resultPromise: Promise[Option[QuickcheckCounterexample]] = Promise()
      val executor = Executors.newWorkStealingPool()

      val futures = for (i <- 1 to threads) yield {
        executor.submit(new Runnable {
          override def run(): Unit = {
            try {
              val r = randomTestsSingle(limit, seed + i, debug, cancellationToken)
              if (r.isDefined) {
                resultPromise.tryComplete(Success(r))
                cancellationToken.set(true)
              }
            } catch {
              case t: Throwable =>
                resultPromise.failure(t)
                cancellationToken.set(true)
            }
          }
        })
      }
      executor.shutdown()
      executor.awaitTermination(timeLimit.toSeconds, TimeUnit.SECONDS)
      resultPromise.tryComplete(Success(None))
      val res = Await.result(resultPromise.future, Duration.Zero)
      res
    } finally {
      cancellationToken.set(true)
    }
  }

  def randomTestsSingle(limit: Int, seed: Int = 0, debug: Boolean = true, cancellationToken: AtomicBoolean): Option[QuickcheckCounterexample] = {
    var startTime = System.nanoTime()

    val ap = new RandomActionProvider(limit, seed, cancellationToken)
    try {
      val state = execute(ap)
      val trace = ap.getTrace()

      if (debug) {
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          println(s"Executed ${state.invocations.size} invocations in ${(System.nanoTime() - startTime) / 1000000}ms and found no counterexample.")
          for (action <- trace) {
            debugLog("  " + action)
          }
          Visualization.printStateGraphToFile(prog, state, s"${prog.name}_${seed}_success")
        }
      }
      None
    } catch {
      case e: InvariantViolationException =>
        val trace = ap.getTrace()

        val (dot, svg) = Visualization.renderStateGraph(prog, e.state)

        Some(QuickcheckCounterexample(
          brokenInvariant = e.inv.source.range,
          info = e.info,
          state = e.state,
          trace = printTrace(trace),
          counterExampleSvg = svg,
          counterExampleDot = dot
        ))
      case e: Throwable =>
        e.printStackTrace()
        throw new RuntimeException("Error in execution: ", e)
    }
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