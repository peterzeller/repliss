package repliss

import crdtver.language.ACrdtInstance.{CrdtInstance, StructInstance}
import crdtver.language.{ACrdtInstance, InputAst}
import crdtver.language.CrdtTypeDefinition.{MapAddCrdt, RegisterCrdt, SetAdd, SetRemove, multiValueRegisterCrdt, MapRemoveCrdt}
import crdtver.language.InputAst.{InAxiomDecl, InCrdtDecl, InInvariantDecl, InOperationDecl, InProcedure, InProgram, InQueryDecl, InTypeDecl, SimpleType}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, LocalState, SnapshotTime, State, TransactionId, WaitForBegin}
import org.scalatest._

/**
  * run with
  * sbt
  * testOnly *CrdtTests
  */
class CrdtQueryTests extends FlatSpec with Matchers {


  "set semantics" should "work with add before remove" in {

    val setCrdt = CrdtInstance(SetRemove(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "add", "x"),
        op(2, "remove", "x")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = evaluateQuery(name = "contains", args = List(AnyValue("x")), state, setCrdt)

    res should equal(AnyValue(false))
  }

  "set semantics" should "work with add after remove" in {

    val setCrdt = CrdtInstance(SetRemove(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "add", "x"),
        op(2, "remove", "x")
      ),
      dependencies = Set(2 -> 1)
    )

    val res = evaluateQuery(name = "contains", args = List(AnyValue("x")), state, setCrdt)

    res should equal(AnyValue(true))
  }

  "set semantics" should "work with add before remove for x and y" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "add", "x"),
        op(2, "remove", "y"),
        op(3, "add", "y"),
        op(4, "remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 3 -> 4, 2 -> 4, 2 -> 3)
    )

    val resx = evaluateQuery(name = "contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(false))

    val resy = evaluateQuery(name = "contains", args = List(AnyValue("y")), state, setCrdt)
    resy should equal(AnyValue(true))
  }

  "set semantics" should "work with add wins semantics for concurrent add and remove for x" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "add", "y"),
        op(2, "add", "x"),
        op(3, "remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val resx = evaluateQuery(name = "contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(true))
  }

  "set semantics" should "work with remove wins semantics for concurrent add and remove for x" in {

    val setCrdt = CrdtInstance(SetRemove(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "add", "y"),
        op(2, "add", "x"),
        op(3, "remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val resx = evaluateQuery(name = "contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(false))
  }


  "set semantics" should "work with add wins semantics for concurrent case" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "remove", "x"),
        op(2, "add", "y"),
        op(3, "add", "x"),
        op(4, "add", "y")
      ),
      dependencies = Set(2 -> 3, 1 -> 3, 3 -> 4)
    )

    val resx = evaluateQuery(name = "contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(true))
    val resy = evaluateQuery(name = "contains", args = List(AnyValue("y")), state, setCrdt)
    resy should equal(AnyValue(true))
  }

  "set semantics" should "work with remove wins semantics for concurrent case of x and y" in {

    val setCrdt = CrdtInstance(SetRemove(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "remove", "x"),
        op(2, "add", "x"),
        op(3, "remove", "x"),
        op(4, "remove", "y"),
        op(5, "add", "y")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 2 -> 4, 2 -> 5, 3 -> 4, 3 -> 5)
    )

    val resx = evaluateQuery(name = "contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(false))
    val resy = evaluateQuery(name = "contains", args = List(AnyValue("y")), state, setCrdt)
    resy should equal(AnyValue(false))
  }


  "register semantics" should "work with get query example 1" in {

    val registerCrdt = CrdtInstance(RegisterCrdt(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "assign", "x"),
        op(2, "assign", "y")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = evaluateQuery(name = "get", args = List(), state, registerCrdt)


    res should equal(AnyValue("y"))
  }

  "register semantics" should "work with get query example 2" in {

    val registerCrdt = CrdtInstance(RegisterCrdt(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "assign", "a"),
        op(2, "assign", "b"),
        op(3, "assign", "c"),
        op(4, "assign", "d")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4)
    )

    val res = evaluateQuery(name = "get", args = List(), state, registerCrdt)


    res should equal(AnyValue("d"))
  }

  "multi value register semantics" should "work with get query" in {

    val registerCrdt = multiValueRegisterCrdt()

    val state = makeState(
      calls = List(
        op(1, "assign", "a"),
        op(2, "assign", "b"),
        op(3, "assign", "c"),
        op(4, "assign", "d")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 2 -> 4)
    )

    val res = registerCrdt.evaluateQuery(name = "get", args = List(), state, null)

    res should equal(AnyValue(List("c","d")))
  }

  "multi value register semantics" should "work with getFirst query" in {

    val registerCrdt = CrdtInstance(multiValueRegisterCrdt(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "assign", "a"),
        op(2, "assign", "b"),
        op(3, "assign", "c"),
        op(4, "assign", "d")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 2 -> 4)
    )

    val res = evaluateQuery(name = "getFirst", args = List(), state, registerCrdt)

    res should equal(AnyValue("c"))
  }

  "multi value register semantics" should "work with contains query" in {

    val registerCrdt = CrdtInstance(multiValueRegisterCrdt(), List(SimpleType("String")), List())

    val state = makeState(
      calls = List(
        op(1, "assign", "a"),
        op(2, "assign", "b"),
        op(3, "assign", "c"),
        op(4, "assign", "d")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 2 -> 4)
    )

    val resFalse = evaluateQuery(name = "mv_contains", args = List(AnyValue("a")), state, registerCrdt)
    val resTrue = evaluateQuery(name = "mv_contains", args = List(AnyValue("d")), state, registerCrdt)

    resFalse should equal(AnyValue(false))
    resTrue should equal(AnyValue(true))


  }

  "map semantics" should "work with contains query add before remove" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id", "x"),
        op(2, "remove", "id", "x")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = evaluateQuery(name = "contains", args = List(AnyValue("id"), AnyValue("x")), state, mapCrdt)

    res should equal(AnyValue(false))
  }

  "map semantics" should "work with contains query remove before add" in {

    val setCrdt = CrdtInstance(SetRemove(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "remove", "id", "x"),
        op(2, "add", "id", "x")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = evaluateQuery(name = "contains", args = List(AnyValue("id"), AnyValue("x")), state, mapCrdt)

    res should equal(AnyValue(true))
  }

  "map semantics" should "work with different ids" in {

    val setCrdt = CrdtInstance(SetRemove(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id0", "x"),
        op(2, "remove", "id1", "y"),
        op(3, "add", "id0", "y"),
        op(4, "remove", "id0", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 3 -> 4, 2 -> 4, 2 -> 3)
    )

    val resy2 = evaluateQuery(name = "contains", args = List(AnyValue("id1"), AnyValue("y")), state, mapCrdt)
    resy2 should equal(AnyValue(false))

    val resx = evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    resx should equal(AnyValue(false))

    val resy = evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue("y")), state, mapCrdt)
    resy should equal(AnyValue(true))


  }

  "map semantics" should "work with different ids 2" in {

    val setCrdt = CrdtInstance(SetRemove(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "k1", "x"),
        op(2, "add", "k2", "y")
      ),
      dependencies = Set()
    )

    val resy1 = evaluateQuery(name = "contains", args = List(AnyValue("k1"), AnyValue("y")), state, mapCrdt)
    resy1 should equal(AnyValue(false))

    val resx1 = evaluateQuery(name = "contains", args = List(AnyValue("k1"), AnyValue("x")), state, mapCrdt)
    resx1 should equal(AnyValue(true))

    val resy2 = evaluateQuery(name = "contains", args = List(AnyValue("k2"), AnyValue("y")), state, mapCrdt)
    resy2 should equal(AnyValue(true))

    val resx2 = evaluateQuery(name = "contains", args = List(AnyValue("k2"), AnyValue("x")), state, mapCrdt)
    resx2 should equal(AnyValue(false))


  }

  "map semantics" should "work with add wins semantics" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "remove", "id1", "x"),
        op(2, "add", "id1", "x"),
        op(3, "delete", "id1"),
        op(4, "add", "id0", "x")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 3 -> 4)
    )

    val res1 = evaluateQuery(name = "contains", args = List(AnyValue("id1"), AnyValue("x")), state, mapCrdt)
    res1 should equal(AnyValue(false))

    val res2 = evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    res2 should equal(AnyValue(true))

  }

  "map semantics" should "work with concurrent add wins semantics" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id0", "x"),
        op(2, "add", "id1", "x"),
        op(3, "delete", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "contains", args = List(AnyValue("id1"), AnyValue("x")), state, mapCrdt)
    res1 should equal(AnyValue(true))

    val res2 = evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    res2 should equal(AnyValue(true))

  }

  "map semantics" should "work with concurrent remove wins semantics" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapRemoveCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id0", "x"),
        op(2, "add", "id1", "x"),
        op(3, "delete", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "contains", args = List(AnyValue("id1"), AnyValue("x")), state, mapCrdt)
    res1 should equal(AnyValue(false))

    val res2 = evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    res2 should equal(AnyValue(true))
  }

  "map semantics" should "work with concurrent remove wins exists query" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapRemoveCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id0", "x"),
        op(2, "add", "id1", "x"),
        op(3, "delete", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "exists", args = List(AnyValue("id1")), state, mapCrdt)
    res1 should equal(AnyValue(false))

  }

  "map semantics" should "work with concurrent add wins exists query" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "add", "id0", "x"),
        op(2, "add", "id1", "x"),
        op(3, "delete", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "exists", args = List(AnyValue("id1")), state, mapCrdt)
    res1 should equal(AnyValue(true))

  }

  "map semantics" should "work with exists query" in {

    val setCrdt = CrdtInstance(SetAdd(), List(SimpleType("String")), List())
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(setCrdt))

    val state = makeState(
      calls = List(
        op(1, "remove", "id1", "x"),
        op(2, "add", "id1", "x"),
        op(3, "delete", "id1"),
        op(4, "add", "id0", "x")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 3 -> 4)
    )

    val res1 = evaluateQuery(name = "exists", args = List(AnyValue("id1")), state, mapCrdt)
    res1 should equal(AnyValue(false))

    val res2 = evaluateQuery(name = "exists", args = List(AnyValue("id0")), state, mapCrdt)
    res2 should equal(AnyValue(true))

  }

  "struct semantics" should "work with contains and get query" in {

    val a = CrdtInstance(SetRemove(), List(SimpleType("String")), List())
    val b = CrdtInstance(RegisterCrdt(), List(SimpleType("String")), List())
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

    val res = evaluateQuery(name = "b_get", args = List(), state, struct)
    res should equal(AnyValue("y"))
  }

  "map struct semantics" should "work with contains and get query" in {

    val a = CrdtInstance(SetRemove(), List(SimpleType("String")), List())
    val b = CrdtInstance(RegisterCrdt(), List(SimpleType("String")), List())
    val struct = StructInstance(Map(
      "a" -> a, "b" -> b
    ))
    val mapCrdt = CrdtInstance(MapAddCrdt(), List(SimpleType("String")), List(struct))

    val state = makeState(
      calls = List(
        op(1, "a_add", "id0","x"),
        op(2, "a_remove", "id0", "x"),
        op(3, "b_assign", "id0","x"),
        op(4, "b_assign", "id1", "y")
      ),
      dependencies = Set(1 -> 2, 3 -> 4)
    )

    val rescontains = evaluateQuery(name = "a_contains", args = List(AnyValue("id0"),AnyValue("x")), state, mapCrdt)
    rescontains should equal(AnyValue(false))

    val resgetx = evaluateQuery(name = "b_get", args = List(AnyValue("id0")), state, mapCrdt)
    resgetx should equal(AnyValue("x"))

    val resgety = evaluateQuery(name = "b_get", args = List(AnyValue("id1")), state, mapCrdt)
    resgety should equal(AnyValue("y"))

  }


  private def evaluateQuery(name: String, args: List[AnyValue], state: State, instance: ACrdtInstance): AnyValue = {
      val res1 = instance.evaluateQuery(name, args, state)
      instance.queryDefinitions().find(q => q.name.name == name) match {
        case Some(query) =>
          // evaluate query
          val prog = InProgram(
            name = "program",
            source = null,
            procedures = List[InProcedure](),
            types = List[InTypeDecl](),
            operations = List[InOperationDecl](),
            queries = List[InQueryDecl](),
            axioms = List[InAxiomDecl](),
            invariants = List[InInvariantDecl](),
            crdts = List[InCrdtDecl](),
            programCrdt = instance
          )
          val interpreter = new Interpreter(prog) {
            override def enumerateValues(t: InputAst.InTypeExpr, state: State): Stream[AnyValue] = t match {
              case SimpleType("String", _) => Stream("x","y","z","a","b","c","d").map(AnyValue)
              case _ => super.enumerateValues(t, state)
            }
          }
          val localState = LocalState(
            varValues = Map(),
            todo = List(),
            waitingFor = WaitForBegin(),
            currentTransaction = None,
            visibleCalls = state.calls.keySet // todo: how to handle visible states
          )
          val validResults = interpreter.evaluateQueryDeclStream(query, args, localState, state)(interpreter.defaultAnyValueCreator)
          assert(validResults.contains(res1), "(wrong evaluate query result)")
        case None =>
          throw new RuntimeException(s"Query $name not defined for $instance. available queries: ${instance.queryDefinitions().map(_.name)}")
      }
      res1
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
