package repliss

import crdtver.language.ACrdtInstance.{CrdtInstance, StructInstance}
import crdtver.language.{ACrdtInstance, CrdtTypeDefinition}
import crdtver.language.CrdtTypeDefinition.{MapCrdt, RegisterCrdt, SetCrdt}
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, SnapshotTime, State, TransactionId}
import org.scalatest._

/**
  * run with
  * sbt
  * testOnly *CrdtTests
  */
class CrdtQueryTests extends FlatSpec with Matchers {


  "set semantics" should "work with add before remove" in {

    val setCrdt = SetCrdt()

    val state = makeState(
      calls = List(
        op(1, "add", "x"),
        op(2, "remove", "x")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = setCrdt.evaluateQuery(name = "contains", args = List(AnyValue("x")), state, null)

    res should equal(AnyValue(false))
  }

  "set semantics" should "work with add after remove" in {

    val setCrdt = SetCrdt()

    val state = makeState(
      calls = List(
        op(1, "add", "x"),
        op(2, "remove", "x")
      ),
      dependencies = Set(2 -> 1)
    )

    val res = setCrdt.evaluateQuery(name = "contains", args = List(AnyValue("x")), state, null)

    res should equal(AnyValue(true))
  }

  "set semantics" should "work with add before remove for x and y" in {

    val setCrdt = SetCrdt()

    val state = makeState(
      calls = List(
        op(1, "add", "x"),
        op(2, "remove", "y"),
        op(3, "add", "y"),
        op(4, "remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 3 -> 4, 2 -> 4, 2 -> 3)
    )

    val resx = setCrdt.evaluateQuery(name = "contains", args = List(AnyValue("x")), state, null)
    resx should equal(AnyValue(false))

    val resy = setCrdt.evaluateQuery(name = "contains", args = List(AnyValue("y")), state, null)
    resy should equal(AnyValue(true))
  }

  "set semantics" should "work with add wins semantics for concurrent add and remove for x" in {

    val setCrdt = SetCrdt()

    val state = makeState(
      calls = List(
        op(1, "add", "y"),
        op(2, "add", "x"),
        op(3, "remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val resx = setCrdt.evaluateQuery(name = "contains", args = List(AnyValue("x")), state, null)
    resx should equal(AnyValue(true))
  }

  "set semantics" should "work with add wins semantics for concurrent case" in {

    val setCrdt = SetCrdt()

    val state = makeState(
      calls = List(
        op(1, "remove", "x"),
        op(2, "add", "y"),
        op(3, "add", "x"),
        op(4, "add", "y")
      ),
      dependencies = Set(2 -> 3, 1 -> 3, 3 -> 4)
    )

    val resx = setCrdt.evaluateQuery(name = "contains", args = List(AnyValue("x")), state, null)
    resx should equal(AnyValue(true))
    val resy = setCrdt.evaluateQuery(name = "contains", args = List(AnyValue("y")), state, null)
    resy should equal(AnyValue(true))
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

    val res = registerCrdt.evaluateQuery(name = "get", args = List(), state, null)


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

    val res = registerCrdt.evaluateQuery(name = "get", args = List(), state, null)


    res should equal(AnyValue("d"))
  }

  "map semantics" should "work with contains query add before remove" in {

    val setCrdt = CrdtInstance(SetCrdt(), List(), List())
    val mapCrdt = CrdtInstance(MapCrdt(), List(), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id", "x"),
        op(2, "remove", "id", "x")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = mapCrdt.definiton.evaluateQuery(name = "contains", args = List(AnyValue("id"), AnyValue("x")), state, mapCrdt)

    res should equal(AnyValue(false))
  }

  "map semantics" should "work with contains query remove before add" in {

    val setCrdt = CrdtInstance(SetCrdt(), List(), List())
    val mapCrdt = CrdtInstance(MapCrdt(), List(), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id", "x"),
        op(2, "remove", "id", "x")
      ),
      dependencies = Set(2 -> 1)
    )

    val res = mapCrdt.definiton.evaluateQuery(name = "contains", args = List(AnyValue("id"), AnyValue("x")), state, mapCrdt)

    res should equal(AnyValue(true))
  }

  "map semantics" should "work with different ids" in {

    val setCrdt = CrdtInstance(SetCrdt(), List(), List())
    val mapCrdt = CrdtInstance(MapCrdt(), List(), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id0", "x"),
        op(2, "remove", "id1", "y"),
        op(3, "add", "id0", "y"),
        op(4, "remove", "id0", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 3 -> 4, 2 -> 4, 2 -> 3)
    )

    val resx = mapCrdt.definiton.evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    resx should equal(AnyValue(false))

    val resy = mapCrdt.definiton.evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue("y")), state, mapCrdt)
    resy should equal(AnyValue(true))

    val resy2 = mapCrdt.definiton.evaluateQuery(name = "contains", args = List(AnyValue("id1"), AnyValue("y")), state, mapCrdt)
    resy2 should equal(AnyValue(false))
  }

  "struct semantics" should "work with contains and get query" in {

    val a = CrdtInstance(SetCrdt(), List(), List())
    val b = CrdtInstance(RegisterCrdt(), List(), List())
    val struct = StructInstance(Map(
      "a" -> a, "b" -> b
    ))

    val state = makeState(
      calls = List(
        op(1, "a_add", "x"),
        op(2, "a_remove", "x"),
        op(3, "b_assign", "x"),
        op(4, "b_assign", "y")
      ),
      dependencies = Set(1 -> 2, 3 -> 4)
    )

    val res = ACrdtInstance.transformcrdt(name = "b_get", args = List(), state, struct)
    res should equal(AnyValue("y"))
  }

  "map struct semantics" should "work with contains and get query" in {

    val a = CrdtInstance(SetCrdt(), List(), List())
    val b = CrdtInstance(RegisterCrdt(), List(), List())
    val struct = StructInstance(Map(
      "a" -> a, "b" -> b
    ))
    val mapCrdt = CrdtInstance(MapCrdt(), List(), List(struct))

    val state = makeState(
      calls = List(
        op(1, "a_add", "id0","x"),
        op(2, "a_remove", "id0", "x"),
        op(3, "b_assign", "id0","x"),
        op(4, "b_assign", "id1", "y")
      ),
      dependencies = Set(1 -> 2, 3 -> 4)
    )

    val resgetx = mapCrdt.definiton.evaluateQuery(name = "b_get", args = List(AnyValue("id0")), state, mapCrdt)
    resgetx should equal(AnyValue("x"))

    val resgety = mapCrdt.definiton.evaluateQuery(name = "b_get", args = List(AnyValue("id1")), state, mapCrdt)
    resgety should equal(AnyValue("y"))

    val rescontains = mapCrdt.definiton.evaluateQuery(name = "a_contains", args = List(AnyValue("id0"),AnyValue("x")), state, mapCrdt)
    rescontains should equal(AnyValue(false))
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
