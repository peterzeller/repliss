package crdtver.testing

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.{Callable, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import crdtver.Repliss
import crdtver.Repliss.QuickcheckCounterexample
import crdtver.language.AtomicTransform
import crdtver.language.InputAst.{AnyType, BoolType, CallIdType, FunctionType, IdType, InProgram, InTypeExpr, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType, UnknownType, UnresolvedType}
import crdtver.testing.Interpreter._
import crdtver.language.InputAst._
import crdtver.utils.{ConcurrencyUtils, Helper}

import scala.collection.immutable.{::, Nil}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Random, Success}

import scala.concurrent.duration._

/**
  * This class is responsible for executing random tests on a Repliss program.
  */
class RandomTester(prog: InProgram) {

  // custom data types can have values 0 <= x < domainSize
  val domainSize = 3

  // maximum number of known ids for generating random values
  val maxUsedIds = 2

  val interpreter = new Interpreter(prog, domainSize)


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
    // trace of executed actions,  newest actions first
    private var exeutedActions = List[Action]()
    private val rand = new Random(seed)
    private var maxGivenId = 0

    def getTrace(): List[Action] = exeutedActions.reverse

    override def discardLast(): Unit = {
      exeutedActions = exeutedActions.tail
    }

    override def nextAction(state: State): Option[Action] = {
      if (cancellationToken.get()) {
        debugLog(s"$seed cancelled execution")
        return None
      }
      if (exeutedActions.size > limit) {
        debugLog(s"$seed found no counter example")
        return None
      }
      val action = randomAction(state)
      exeutedActions = action :: exeutedActions
      debugLog(s"$seed generating action ${exeutedActions.size}/$limit")
      Some(action)
    }


    def newRandomInvariantCheck(state: State): Action = {
      val id = pickRandom(state.localStates.keys.toList)(rand)
      InvariantCheck(id)
    }

    def randomAction(state: State): Action = {
      if (state.localStates.nonEmpty && exeutedActions.size == limit) {
        newRandomInvariantCheck(state)
      }


      val waitFors = getLocalWaitingFors(state)
      val i = rand.nextInt(waitFors.size + 1)
      if (i >= waitFors.size) {
        if (state.localStates.nonEmpty && rand.nextDouble() < 0.1) {
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
      val ls: LocalState = state.localStates.getOrElse(invoc, return Set())

      // first: determine how many transactions to base this on:
      var count: Int = 1 + rand.nextInt(2)

      lazy val firstTransaction = !allTransactions.exists(tx => state.transactions(tx).origin == invoc)
      if (rand.nextDouble() < 0.1 && firstTransaction) {
        // some first transactions can be based on no transactions
        count -= 1
      }


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
      case WaitForNewId(_, _) =>
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
        case SimpleType(name, _source) =>
          // TODO handle datatypes
          Some(Interpreter.domainValue(name, rand.nextInt(domainSize)))
        case idt@IdType(_name, _source) =>
          knownIds.get(idt) match {
            case Some(s) =>
              // only pick from the first N (maxUsedIds) unique identifiers to make it more likely that we work on the same data:
              val ids = s.toList.sortBy(_.value.toString).take(maxUsedIds)
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





  def renderStateGraph(state: State): (String, String) = {
    val sb = new StringBuilder()

    def p(s: String): Unit = {
      sb.append(s)
      sb.append("\n")
    }

    printStateGraph(state, p)
    val dot = sb.toString()
    val dotIs = new ByteArrayInputStream(dot.getBytes(StandardCharsets.UTF_8))
    import sys.process._

    var reducedDot: String = ("tred" #< dotIs).!!

    sb.clear()
    sb.append(reducedDot.substring(0, reducedDot.length - 2))
    addInvocationDependencies(state, p)
    sb.append("}")
    reducedDot = sb.toString()


    val dotIs2 = new ByteArrayInputStream(reducedDot.getBytes(StandardCharsets.UTF_8))

    val output: String = ("dot -Tsvg" #< dotIs2).!!
//    debugLog(s"SVG output = $output")
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
    (s"tred model/graph_$filename.dot" #| s"dot -Tsvg -o model/graph_$filename.svg").!

    (s"tred model/graph_$filename.dot" #| s"dot -Tpdf -o model/graph_$filename.pdf").!
  }

  def printStateGraph(state: State, p: (String) => Unit): Unit = {

    p(s"digraph G {")
    p("   graph [splines=true overlap=false]")
    for (i <- state.invocations.values) {
      var containsCall = false
      p(
        s"""subgraph cluster_${i.id} {
           |   style="rounded,filled,dashed";
           |   color="#000000";
           |   fillcolor="#eeeeff";
           |   node [style="filled,dashed", color=white]
           |   label = "${i.id}
           |      ${i.operation}
           |      result: ${i.result.getOrElse("-")}"
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
             |   style="rounded,filled,dotted";
             |   color="#000000";
             |   fillcolor="#eeffee";
             |   node [style="filled,dotted", color=white, shape=box, style="rounded,filled"]
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

          p(s"""${c.id}[label="${c.id}\n$opStr", style="rounded,filled,solid", color="#666666", fillcolor="#ffffff"];""")
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


  private def addInvocationDependencies(state: State, p: (String) => Unit) = {
    // add invisible-id dependencies:
    val callsInInvocation: Map[InvocationId, Seq[CallId]] = state.calls.values
      .groupBy(ci => ci.origin)
      .mapValues(calls => calls.map(_.id).toSeq.sorted)


    var idOrigin = Map[(IdType, AnyValue), InvocationId]()
    val sortedInvocations: Seq[InvocationInfo] = state.invocations.values.toList.sortBy(_.id)
    for (invoc <- sortedInvocations) {
      invoc.result match {
        case Some(AnyValue(DataTypeValue(_, List(res)))) =>
          val proc = prog.findProcedure(invoc.operation.operationName)
          val returnedIds = interpreter.extractIds(res, proc.returnType)
          for ((idt, idvals) <- returnedIds; idval <- idvals) {
            if (!idOrigin.contains((idt, idval))) {
              idOrigin += ((idt, idval) -> invoc.id)
            }
          }
        case None =>
      }
    }


    for (invoc <- sortedInvocations) {
      val proc = prog.findProcedure(invoc.operation.operationName)
      val argIds = interpreter.extractIdsList(invoc.operation.args, proc.params.map(_.typ))
      for ((idt, idvals) <- argIds; idval <- idvals) {

        idOrigin.get((idt, idval)) match {
          case Some(id) =>
            for {
              calls1 <- callsInInvocation.get(id)
              c1 <- calls1.lastOption
              calls2 <- callsInInvocation.get(invoc.id)
              c2 <- calls2.headOption
            } {
              //              println("$c1 -> $c2;")
              p(s"""  $c1 -> $c2 [style=invis];""")
            }
          case None =>
        }
      }
    }
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
          printStateGraphToFile(state, s"${prog.name}_${seed}_success")
        }
      }
      None
    } catch {
      case e1: InvariantViolationException =>
        val trace = ap.getTrace()

        if (debug) {
          println(s"Found counter example with ${e1.state.invocations.size} invocations in ${(System.nanoTime() - startTime) / 1000000}ms")
        }

        startTime = System.nanoTime()
        val (smallTrace, smallState, e) = tryShrink(trace, e1.state, e1, cancellationToken)

        if (debug) {
          println(s"Reduced to example with ${smallState.invocations.size} invocations in ${(System.nanoTime() - startTime) / 1000000}ms")
          for (action <- smallTrace) {
            debugLog("  " + action)
          }
          debugLog(s"reduced from ${trace.size} to ${smallTrace.size} actions")
          printStateGraphToFile(e.state, s"${prog.name}_${seed}_original")
          printStateGraphToFile(smallState, s"${prog.name}_${seed}_shrunk")
        }

        val (dot, svg) = renderStateGraph(smallState)

        Some(QuickcheckCounterexample(
          brokenInvariant = e.inv.source.range,
          info = e.info,
          state = e.state,
          trace = printTrace(smallTrace),
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

  // @tailrec TODO why not?
  private def tryShrink(trace: List[Action], lastState: State, lastException: InvariantViolationException, cancellationToken: AtomicBoolean): (List[Action], State, InvariantViolationException) = {
    if (cancellationToken.get()) {
      // process cancelled
      return (trace, lastState, lastException)
    }
    debugLog(s"shrinking trace with ${trace.length} elements")
    for (i <- trace.indices) {
      var shrunkTrace = removeAtIndex(trace, i)
      trace(i) match {
        case CallAction(invocationId, _, _) =>
          var hasInvCheck = false
          var snapshot = Set[TransactionId]()
          import scala.util.control.Breaks._
          breakable {
            for (a <- trace) {
              a match {
                case InvariantCheck(id) if id == invocationId =>
                  hasInvCheck = true
                  break
                case LocalAction(id, StartTransaction(txid, pulled)) if id == invocationId =>
                  snapshot = snapshot ++ pulled
                case _ =>
              }
            }
          }
          if (hasInvCheck) {
            // if there is an invariant check in this invocation, try to move it to previous invocations:
            shrunkTrace = shrunkTrace.flatMap {
              case LocalAction(id, StartTransaction(txid, pulled)) if snapshot.contains(txid) =>
                List(
                  LocalAction(id, StartTransaction(txid, pulled)),
                  InvariantCheck(id)
                )
              case a => List(a)
            }
          }
        case _ =>
        // ignore
      }

      debugLog(s"Trying to remove action ${trace(i)}")
      tryShrunkTrace(shrunkTrace) match {
        case Some((tr, e)) =>
          debugLog("Shrinking successful")
          return tryShrink(tr, e.state, e, cancellationToken)
        case None =>
          debugLog("Shrinking failed")
        // continue
      }
    }
    (trace, lastState, lastException)
  }

  def tryShrunkTrace(trace: List[Action]): Option[(List[Action], InvariantViolationException)] = {
    val ap = new TraceActionProvider(trace)
    try {
      val state = execute(ap)
      None
    } catch {
      case e: InvariantViolationException =>
        Some((ap.getTrace(), e))
    }
  }


  def execute(actionProvider: ActionProvider): State = {
    var state = State()
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


object RandomTesterTest {
  def main(args: Array[String]): Unit = {
    //    val input = Helper.getResource("/examples/userbase_fail3.rpls")
    val input = Helper.getResource("/examples/userbase_fail1.rpls")
    //    val input = Helper.getResource("/examples/tournament.rpls")
    val typed = Repliss.parseAndTypecheck("interpretertest", input)
    val prog = AtomicTransform.transformProg(typed.get())

    println("prog: ---")
    println(prog.procedures.map(p => s"$p\n${p.body}\n\n").mkString("\n\n"))
    println("-----")


    println("tests start")
    val tester = new RandomTester(prog)
    val result: Option[QuickcheckCounterexample] = tester.randomTests(limit = 800, threads = 1)

    result match {
      case Some(ce) =>
        println(s"Counter example:\n$ce")
      case None =>
        println("no counterexample found")

    }

    println("tests done")
  }
}