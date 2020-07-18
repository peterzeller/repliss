package repliss

import crdtver.RunArgs
import crdtver.language.TypedAst
import crdtver.language.crdts.{ACrdtInstance, FlagCrdt, MVRegisterCrdt, MapCrdt, RegisterCrdt, SetCrdt, StructCrdt}
import crdtver.language.TypedAst.{InAxiomDecl, InCrdtDecl, InInvariantDecl, InOperationDecl, InProcedure, InProgram, InQueryDecl, InTypeDecl, InTypeExpr, SimpleType, Subst, TypeVarUse}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, LocalState, SnapshotTime, State, TransactionId, WaitForBegin}
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

/**
  * run with
  * sbt
  * testOnly *CrdtTests
  */
class CrdtQueryTests extends AnyFlatSpec with org.scalatest.matchers.should.Matchers {

  private val stringType: SimpleType = SimpleType("String", List())()
  private val intType: SimpleType = SimpleType("Int", List())()


  private val setCrdt_rw = new SetCrdt(FlagCrdt.DW(), "Set_rw").instantiate(List(stringType), List())

  private val setCrdt_aw = new SetCrdt(FlagCrdt.EW(), "Set_aw").instantiate(List(stringType), List())
  
  private val registerCrdt = new RegisterCrdt().instantiate(List(stringType), List())

  private val mvRegisterCrdt = new MVRegisterCrdt().instantiate(List(stringType), List())

  private def mapCrdt_uw(v: ACrdtInstance) =
    new MapCrdt(FlagCrdt.EW(), MapCrdt.DeleteAffectsPrior(), "Map_uw")
      .instantiate(List(stringType), List(v))

  private def mapCrdt_dw(v: ACrdtInstance) =
      new MapCrdt(FlagCrdt.DW(), MapCrdt.DeleteAffectsPriorAndConcurrent(), "Map_dw")
        .instantiate(List(stringType), List(v))


  /**
   * Checks that the Interpreter state contains correct operations
   * with respect to the types defined for the CRDT
   **/
  def typeCheckState(crdt: ACrdtInstance, state: State): Unit = {

    val dataTypes = crdt.additionalDataTypesRec

    def checkTypeA(op: AnyValue, t: InTypeExpr): Unit = op.value match {
      case d: DataTypeValue =>
        checkType(d, t)
      case v =>
        if (v.getClass.getSimpleName != t.toString)
          println(s"WARNING: $v (${v.getClass}) does not match $t")
    }

    def checkType(op: DataTypeValue, t: InTypeExpr): Unit = {
      t match {
        case SimpleType(name, typeArgs) =>
          val dt = dataTypes.find(_.name.name == name)
            .getOrElse(throw new Exception(s"Type $name not found"))
          val dtCase = dt.dataTypeCases.find(_.name.name == op.operationName)
            .getOrElse(throw new Exception(s"Case ${op.operationName} not found in ${dt.dataTypeCases.map(_.name.name)}."))
          if (dtCase.params.length != op.args.length) {
            throw new RuntimeException(s"Wrong number of arguments. Expected ${dtCase.params.length} but got ${op.args.length}")
          }
          val subst = Subst(dt.typeParameters.map(tp => TypeVarUse(tp.name.name)()).zip(typeArgs).toMap)

          for ((a,p) <- op.args.zip(dtCase.params)) {
            checkTypeA(a, p.typ.subst(subst))
          }
        case _ => throw new Exception(s"Operation $op does not match type $t (${t.getClass})")
      }
    }


    for (crdtInfo <- state.calls.values) {
      val op = crdtInfo.operation
      checkType(op, crdt.operationType)
    }

  }

  "set semantics" should "work with add before remove" in {

    val setCrdt = setCrdt_rw

    val state = makeState(
      calls = List(
        op(1, "Add", "x"),
        op(2, "Remove", "x")
      ),
      dependencies = Set(1 -> 2)
    )

    typeCheckState(setCrdt, state)

    val res = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)

    res should equal(AnyValue(false))
  }

  "set semantics" should "work with add after remove" in {

    val setCrdt = setCrdt_rw

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

    val setCrdt = setCrdt_aw

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

    val setCrdt = setCrdt_aw

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

    val setCrdt = setCrdt_rw

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

    val setCrdt = setCrdt_aw

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

    val setCrdt = setCrdt_rw

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

    val registerCrdt = mvRegisterCrdt

    val state = makeState(
      calls = List(
        op(1, "assign", "a"),
        op(2, "assign", "b"),
        op(3, "assign", "c"),
        op(4, "assign", "d")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 2 -> 4)
    )

    val res = registerCrdt.evaluateQuery(name = "get", args = List(), state)

    res should equal(AnyValue(List("c","d")))
  }

  "multi value register semantics" should "work with getFirst query" in {

    val registerCrdt = mvRegisterCrdt

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

    val registerCrdt = mvRegisterCrdt

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

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

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

    val mapCrdt = mapCrdt_uw(setCrdt_rw)

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

    val mapCrdt = mapCrdt_uw(setCrdt_rw)

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

    val mapCrdt = mapCrdt_uw(setCrdt_rw)

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

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

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

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

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

    val mapCrdt = mapCrdt_dw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "add", "id0", 101),
        op(2, "add", "id1", 101),
        op(3, "delete", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "contains", args = List(AnyValue("id1"), AnyValue(101)), state, mapCrdt)
    res1 should equal(AnyValue(false))

    val res2 = evaluateQuery(name = "contains", args = List(AnyValue("id0"), AnyValue(101)), state, mapCrdt)
    res2 should equal(AnyValue(true))
  }

  "map semantics" should "work with concurrent remove wins exists query" in {

    val mapCrdt = mapCrdt_dw(setCrdt_aw)

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

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "add", "id0", 101),
        op(2, "add", "id1", 101),
        op(3, "delete", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "exists", args = List(AnyValue("id1")), state, mapCrdt)
    res1 should equal(AnyValue(true))

  }

  "map semantics" should "work with exists query" in {

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

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

    val a = setCrdt_rw
    val b = registerCrdt
    val struct = new StructCrdt("struct", Map(
      "a" -> a, "b" -> b
    )).instantiate()

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

    val a = setCrdt_rw
    val b = registerCrdt
    val struct = new StructCrdt("struct", Map(
      "a" -> a, "b" -> b
    )).instantiate()
    val mapCrdt = mapCrdt_uw(struct)

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
            axioms = List[InAxiomDecl](),
            invariants = List[InInvariantDecl](),
            programCrdt = instance
          )
          val interpreter = new Interpreter(prog, RunArgs()) {
            override def enumerateValues(t: TypedAst.InTypeExpr, state: State): LazyList[AnyValue] = t match {
              case SimpleType("String", List()) => LazyList("x","y","z","a","b","c","d").map(AnyValue)
              case SimpleType("Int", List()) => LazyList(1,2,3,4,101,102,103,104).map(AnyValue)
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
          val validResults = interpreter.evaluateQueryDeclLazyList(query, args, localState, state)(interpreter.defaultAnyValueCreator)
          res1 match {
            case Some(res) =>
              assert(validResults.contains(res), "(wrong evaluate query result)")
              res
            case None =>
              validResults.head
          }
        case None =>
          throw new RuntimeException(s"Query $name not defined for $instance. available queries: ${instance.queryDefinitions().map(_.name)}")
      }
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
