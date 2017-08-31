package repliss

import crdtver.language.CrdtTypeDefinition.RegisterCrdt
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, SnapshotTime, State, TransactionId}
import org.scalatest._

/**
  * run with
  * sbt
  * testOnly *CrdtTests
  */
class CrdtQueryTests extends FlatSpec with Matchers {


  "set semantics" should "work with add before remove" in {

    val registerCrdt = RegisterCrdt()

    val state = makeState(
      calls = List(
        op(1, "add", "x"),
        op(2, "remove", "x")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = registerCrdt.evaluateQuery(name = "contains", args = List(AnyValue("x")), state)

    res should equal(AnyValue(false))
  }

  "set semantics" should "work with add after remove" in {

      val registerCrdt = RegisterCrdt()

      val state = makeState(
        calls = List(
          op(1, "add", "x"),
          op(2, "remove", "x")
        ),
        dependencies = Set(2 -> 1)
      )

      val res = registerCrdt.evaluateQuery(name = "contains", args = List(AnyValue("x")), state)

      res should equal(AnyValue(true))
    }


  "register semantics" should "work with get query example 1" in {

    val registerCrdt = RegisterCrdt()

    val state = makeState(
      calls = List(
        op(1, "assign", "x"),
        op(2, "assign", "y")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = registerCrdt.evaluateQuery(name = "get", args = List(), state)


    res should equal(AnyValue("y"))
  }

  "register semantics" should "work with get query example 2" in {

    val registerCrdt = RegisterCrdt()

    val state = makeState(
      calls = List(
        op(1, "assign", "a"),
        op(2, "assign", "b"),
        op(3, "assign", "c"),
        op(4, "assign", "d")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4)
    )

    val res = registerCrdt.evaluateQuery(name = "get", args = List(), state)


    res should equal(AnyValue("d"))
  }


  def op(callId: Int, name: String, args: Any*) =
    Op(CallId(callId), name, args.toList.map(AnyValue))

  case class Op(callId: CallId, name: String, args: List[AnyValue])

  def calculateDependencies(callId: CallId, dependencies: Set[(CallId, CallId)]): Set[CallId] = {
    var result = Set(callId)

    while (true) {
      val newDependencies = for ((x, y) <- dependencies; if result contains y) yield x
      val newResult = result ++ newDependencies
      if (newResult.size == result.size) {
        return result
      }
      result = newResult
    }
    return result
  }

  def makeState(calls: List[Op], dependencies: Set[(Int, Int)]): State =
    makeStateCallIds(calls, dependencies.map { case (x, y) => (CallId(x), CallId(y)) })

  def makeStateCallIds(calls: List[Op], dependencies: Set[(CallId, CallId)]): State = {

    val callMap: List[(CallId, CallInfo)] =
      for (c <- calls) yield {
        val time = calculateDependencies(c.callId, dependencies)
        c.callId -> CallInfo(
          id = c.callId,
          operation = DataTypeValue(c.name, c.args),
          callClock = SnapshotTime(time),
          callTransaction = TransactionId(1),
          origin = InvocationId(1)
        )
      }

    State(
      calls = callMap.toMap
    )
  }


}
