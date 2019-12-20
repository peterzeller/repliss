package crdtver.testing

import java.util.concurrent.atomic.AtomicBoolean

import crdtver.Repliss.QuickcheckCounterexample
import crdtver.RunArgs
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionType, IdType, InProgram, InTypeExpr, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType, _}
import crdtver.testing.Interpreter._
import crdtver.testing.Visualization.RenderResult
import crdtver.utils.LazyListUtils
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

//    val keys: LazyList[InvocationId] = state.localStates.keys.to(LazyList)
//    keys.map(id => InvariantCheck(id))

    state.localStates.keys.maxByOption(_.id).map(id => InvariantCheck(id)).to(LazyList)

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
      case SimpleType(name) =>
        // TODO handle datatypes
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


  def randomTestsSingle(limit: Int, debug: Boolean = true, sequentialMode: Boolean = true): Option[QuickcheckCounterexample] = {
    val interpreter = new Interpreter(prog, RunArgs(), domainSize = 2)


    case class S(state: State, reverseTrace: List[Action], ive: Option[InvariantViolationException] = None)

    val initialState = S(State(interpreter = Some(interpreter)), List())

    def executeAction(s: S, action: Action): Option[S] = {
      try {
        interpreter.executeAction(s.state, action) match {
          case Some(newState) =>
            val newTrace = action::s.reverseTrace
            if (newTrace.length > 3
              && newTrace.exists { case CallAction(InvocationId(1), "sendMessage", _) => true case _ => false}
              && newTrace.exists { case CallAction(_, "editMessage", _) => true case _ => false}
              && newTrace.exists { case CallAction(_, "deleteMessage", _) => true case _ => false}
              && newTrace.exists { case CallAction(InvocationId(n), "getMessage", _) => n >= 4 case _ => false}
            ) {
              for ((t, i) <- newTrace.reverse.zipWithIndex) {
                println(s" $i. ${t.print}")
              }
            }
            interpreter.checkInvariants(newState)

            Some(S(newState, newTrace, None))
          case None =>
            None
        }
      } catch {
        case ive: InvariantViolationException =>
          println(s"Found counter example ...")
          Some(s.copy(ive = Some(ive)))
        case e: Throwable =>
          val msg =
            s"""
               |Error ${e.getMessage}
               |when evaluating $action
               |in state ${s.state}"
               |with trace: ${s.reverseTrace.reverse.mkString("\n")}
               |""".stripMargin
          throw new RuntimeException(msg, e)
      }

    }

    def children(s: S): LazyList[S] = {
      if (s.ive.isDefined)
        LazyList()
      else
        for {
          action <- possibleActions(s.state, sequentialMode)
          newState <- executeAction(s, action)
        } yield newState
    }

    TreeWalker.walkTree4[S](TreeWalker.tree(initialState, children), breadth = 2)
      .takeWhile(_ => !Thread.currentThread().isInterrupted)
      .find((s: S) => s.ive.isDefined)
      .map((s: S) => {
        println("FOUND counter example")
        val e = s.ive.get
        val renderResult = Visualization.renderStateGraph(prog, e.state)

        QuickcheckCounterexample(
          brokenInvariant = e.inv.source.range,
          info = e.info,
          state = e.state,
          trace = printTrace(s.reverseTrace.reverse),
          counterExampleRender = renderResult
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