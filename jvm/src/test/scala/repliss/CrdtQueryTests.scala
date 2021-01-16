package repliss

import crdtver.RunArgs
import crdtver.language.InputAst.NoSource
import crdtver.language.TypedAst
import crdtver.language.crdts.{ACrdtInstance, FlagCrdt, MVRegisterCrdt, MapCrdt, RegisterCrdt, SetCrdt, StructCrdt}
import crdtver.language.TypedAst.{CallInfoType, InAxiomDecl, InCrdtDecl, InInvariantDecl, InOperationDecl, InProcedure, InProgram, InQueryDecl, InTypeDecl, InTypeExpr, SimpleType, Subst, TypeVarUse}
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

          for ((a, p) <- op.args.zip(dtCase.params)) {
            checkTypeA(a, p.typ.subst(subst))
          }
        case CallInfoType() =>
          op match {
            case DataTypeValue("Op", List(AnyValue(o: DataTypeValue))) =>
              checkType(o, crdt.operationType)
          }
        case _ => throw new Exception(s"Operation $op does not match type $t (${t.getClass})")
      }
    }


    for (crdtInfo <- state.calls.values) {
      val op = crdtInfo.operation
      checkType(op, TypedAst.CallInfoType())
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


    val res = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)

    res should equal(AnyValue(false))
  }

  "set semantics" should "work with add after remove" in {

    val setCrdt = setCrdt_rw

    val state = makeState(
      calls = List(
        op(1, "Add", "x"),
        op(2, "Remove", "x")
      ),
      dependencies = Set(2 -> 1)
    )

    val res = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)

    res should equal(AnyValue(true))
  }

  "set semantics" should "work with add before remove for x and y" in {

    val setCrdt = setCrdt_aw

    val state = makeState(
      calls = List(
        op(1, "Add", "x"),
        op(2, "Remove", "y"),
        op(3, "Add", "y"),
        op(4, "Remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 3 -> 4, 2 -> 4, 2 -> 3)
    )

    val resx = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(false))

    val resy = evaluateQuery(name = "Contains", args = List(AnyValue("y")), state, setCrdt)
    resy should equal(AnyValue(true))
  }

  "set semantics" should "work with add wins semantics for concurrent add and remove for x" in {

    val setCrdt = setCrdt_aw

    val state = makeState(
      calls = List(
        op(1, "Add", "y"),
        op(2, "Add", "x"),
        op(3, "Remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val resx = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(true))
  }

  "set semantics" should "work with remove wins semantics for concurrent add and remove for x" in {

    val setCrdt = setCrdt_rw

    val state = makeState(
      calls = List(
        op(1, "Add", "y"),
        op(2, "Add", "x"),
        op(3, "Remove", "x")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val resx = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(false))
  }


  "set semantics" should "work with add wins semantics for concurrent case" in {

    val setCrdt = setCrdt_aw

    val state = makeState(
      calls = List(
        op(1, "Remove", "x"),
        op(2, "Add", "y"),
        op(3, "Add", "x"),
        op(4, "Add", "y")
      ),
      dependencies = Set(2 -> 3, 1 -> 3, 3 -> 4)
    )

    val resx = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(true))
    val resy = evaluateQuery(name = "Contains", args = List(AnyValue("y")), state, setCrdt)
    resy should equal(AnyValue(true))
  }

  "set semantics" should "work with remove wins semantics for concurrent case of x and y" in {

    val setCrdt = setCrdt_rw

    val state = makeState(
      calls = List(
        op(1, "Remove", "x"),
        op(2, "Add", "x"),
        op(3, "Remove", "x"),
        op(4, "Remove", "y"),
        op(5, "Add", "y")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 2 -> 4, 2 -> 5, 3 -> 4, 3 -> 5)
    )

    val resx = evaluateQuery(name = "Contains", args = List(AnyValue("x")), state, setCrdt)
    resx should equal(AnyValue(false))
    val resy = evaluateQuery(name = "Contains", args = List(AnyValue("y")), state, setCrdt)
    resy should equal(AnyValue(false))
  }


  "register semantics" should "work with get query example 1" in {


    val state = makeState(
      calls = List(
        op(1, "Assign", "x"),
        op(2, "Assign", "y")
      ),
      dependencies = Set(1 -> 2)
    )

    val res = evaluateQuery(name = "ReadRegister", args = List(), state, registerCrdt)


    res should equal(AnyValue("y"))
  }

  "register semantics" should "work with get query example 2" in {


    val state = makeState(
      calls = List(
        op(1, "Assign", "a"),
        op(2, "Assign", "b"),
        op(3, "Assign", "c"),
        op(4, "Assign", "d")
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4)
    )

    val res = evaluateQuery(name = "ReadRegister", args = List(), state, registerCrdt)


    res should equal(AnyValue("d"))
  }

  "multi value register semantics" should "work with get query" in {

    val registerCrdt = mvRegisterCrdt

    val state = makeState(
      calls = List(
        op(1, "Assign", "a"),
        op(2, "Assign", "b"),
        op(3, "Assign", "c"),
        op(4, "Assign", "d")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 2 -> 4)
    )

    val res = evaluateQuerySpec(name = "ReadFirst", args = List(), state, registerCrdt)

    res should equal(LazyList(AnyValue("c"), AnyValue("d")))
  }

  "multi value register semantics" should "work with getFirst query" in {

    val registerCrdt = mvRegisterCrdt

    val state = makeState(
      calls = List(
        op(1, "Assign", "a"),
        op(2, "Assign", "b"),
        op(3, "Assign", "c"),
        op(4, "Assign", "d")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 2 -> 4)
    )

    val res = evaluateQuery(name = "ReadFirst", args = List(), state, registerCrdt)

    res should equal(AnyValue("c"))
  }

  "multi value register semantics" should "work with contains query" in {

    val registerCrdt = mvRegisterCrdt

    val state = makeState(
      calls = List(
        op(1, "Assign", "a"),
        op(2, "Assign", "b"),
        op(3, "Assign", "c"),
        op(4, "Assign", "d")
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 2 -> 4)
    )

    val resFalse = evaluateQuery(name = "MvContains", args = List(AnyValue("a")), state, registerCrdt)
    val resTrue = evaluateQuery(name = "MvContains", args = List(AnyValue("d")), state, registerCrdt)

    resFalse should equal(AnyValue(false))
    resTrue should equal(AnyValue(true))


  }

  "map semantics" should "work with contains query add before remove" in {

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id", o("Add", "x")),
        op(2, "NestedOp", "id", o("Remove", "x"))
      ),
      dependencies = Set(1 -> 2)
    )

    val res = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id"), AnyValue("x")), state, mapCrdt)

    res should equal(AnyValue(false))
  }

  "map semantics" should "work with contains query remove before add" in {

    val mapCrdt = mapCrdt_uw(setCrdt_rw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id", o("Remove", "x")),
        op(2, "NestedOp", "id", o("Add", "x"))
      ),
      dependencies = Set(1 -> 2)
    )

    val res = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id"), AnyValue("x")), state, mapCrdt)

    res should equal(AnyValue(true))
  }

  "map semantics" should "work with different ids" in {

    val mapCrdt = mapCrdt_uw(setCrdt_rw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id0", o("Add", "x")),
        op(2, "NestedOp", "id1", o("Remove", "y")),
        op(3, "NestedOp", "id0", o("Add", "y")),
        op(4, "NestedOp", "id0", o("Remove", "x"))
      ),
      dependencies = Set(1 -> 2, 1 -> 3, 3 -> 4, 2 -> 4, 2 -> 3)
    )

    val resy2 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id1"), AnyValue("y")), state, mapCrdt)
    resy2 should equal(AnyValue(false))

    val resx = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    resx should equal(AnyValue(false))

    val resy = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id0"), AnyValue("y")), state, mapCrdt)
    resy should equal(AnyValue(true))


  }

  "map semantics" should "work with different ids 2" in {

    val mapCrdt = mapCrdt_uw(setCrdt_rw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "k1", o("Add", "x")),
        op(2, "NestedOp", "k2", o("Add", "y"))
      ),
      dependencies = Set()
    )

    val resy1 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("k1"), AnyValue("y")), state, mapCrdt)
    resy1 should equal(AnyValue(false))

    val resx1 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("k1"), AnyValue("x")), state, mapCrdt)
    resx1 should equal(AnyValue(true))

    val resy2 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("k2"), AnyValue("y")), state, mapCrdt)
    resy2 should equal(AnyValue(true))

    val resx2 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("k2"), AnyValue("x")), state, mapCrdt)
    resx2 should equal(AnyValue(false))


  }

  "map semantics" should "work with add wins semantics" in {

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id1", o("Remove", "x")),
        op(2, "NestedOp", "id1", o("Add", "x")),
        op(3, "DeleteKey", "id1"),
        op(4, "NestedOp", "id0", o("Add", "x"))
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 3 -> 4)
    )

    val res1 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id1"), AnyValue("x")), state, mapCrdt)
    res1 should equal(AnyValue(false))

    val res2 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    res2 should equal(AnyValue(true))

  }

  "map semantics" should "work with concurrent add wins semantics" in {

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id0", o("Add", "x")),
        op(2, "NestedOp", "id1", o("Add", "x")),
        op(3, "DeleteKey", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id1"), AnyValue("x")), state, mapCrdt)
    res1 should equal(AnyValue(true))

    val res2 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    res2 should equal(AnyValue(true))

  }

  "map semantics" should "work with concurrent remove wins semantics" in {

    val mapCrdt = mapCrdt_dw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id0", o("Add", "x")),
        op(2, "NestedOp", "id1", o("Add", "x")),
        op(3, "DeleteKey", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id1"), AnyValue("x")), state, mapCrdt)
    res1 should equal(AnyValue(false))

    val res2 = evaluateQuery(name = "NestedQuery_Contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    res2 should equal(AnyValue(true))
  }

  "map semantics" should "work with concurrent remove wins exists query" in {

    val mapCrdt = mapCrdt_dw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id0", o("Add", "x")),
        op(2, "NestedOp", "id1", o("Add", "x")),
        op(3, "DeleteKey", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "ContainsKey", args = List(AnyValue("id1")), state, mapCrdt)
    res1 should equal(AnyValue(false))

  }

  "map semantics" should "work with concurrent add wins exists query" in {

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id0", o("Add", "x")),
        op(2, "NestedOp", "id1", o("Add", "x")),
        op(3, "DeleteKey", "id1")
      ),
      dependencies = Set(1 -> 2, 1 -> 3)
    )

    val res1 = evaluateQuery(name = "ContainsKey", args = List(AnyValue("id1")), state, mapCrdt)
    res1 should equal(AnyValue(true))

  }

  "map semantics" should "work with exists query" in {

    val mapCrdt = mapCrdt_uw(setCrdt_aw)

    val state = makeState(
      calls = List(
        op(1, "NestedOp", "id1", o("Remove", "x")),
        op(2, "NestedOp", "id1", o("Add", "x")),
        op(3, "DeleteKey", "id1"),
        op(4, "NestedOp", "id0", o("Add", "x"))
      ),
      dependencies = Set(1 -> 2, 2 -> 3, 3 -> 4)
    )

    val res1 = evaluateQuery(name = "ContainsKey", args = List(AnyValue("id1")), state, mapCrdt)
    res1 should equal(AnyValue(false))

    val res2 = evaluateQuery(name = "ContainsKey", args = List(AnyValue("id0")), state, mapCrdt)
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
        op(1, "a", o("Add", "x")),
        op(2, "a", o("Remove", "x")),
        op(3, "b", o("Assign", "x")),
        op(4, "b", o("Assign", "y"))
      ),
      dependencies = Set(1 -> 2, 3 -> 4)
    )

    val res = evaluateQuery(name = "bQry_ReadRegister", args = List(), state, struct)
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
        op(1, "NestedOp", "id0", o("a", o("Add", "x"))),
        op(2, "NestedOp", "id0", o("a", o("Remove", "x"))),
        op(3, "NestedOp", "id0", o("b", o("Assign", "x"))),
        op(4, "NestedOp", "id1", o("b", o("Assign", "y")))
      ),
      dependencies = Set(1 -> 2, 3 -> 4)
    )

    val rescontains = evaluateQuery(name = "NestedQuery_aQry_Contains", args = List(AnyValue("id0"), AnyValue("x")), state, mapCrdt)
    rescontains should equal(AnyValue(false))

    val resgetx = evaluateQuery(name = "NestedQuery_bQry_ReadRegister", args = List(AnyValue("id0")), state, mapCrdt)
    resgetx should equal(AnyValue("x"))

    val resgety = evaluateQuery(name = "NestedQuery_bQry_ReadRegister", args = List(AnyValue("id1")), state, mapCrdt)
    resgety should equal(AnyValue("y"))

  }


  private def evaluateQuery(name: String, args: List[AnyValue], state: State, instance: ACrdtInstance): AnyValue = {

    val state2: State = makeState(state, instance)
    val specResults = evaluateQuerySpec(name, args, state, instance)

    val res1 = instance.evaluateQuery(name, args, state2)
    res1 match {
      case Some(res) =>
        assert(specResults.contains(res), "(wrong evaluate query result)")
        res
      case None =>
//        println(s"results = ${specResults.toList}")
        specResults.head
    }
  }


  private def evaluateQuerySpec(name: String, args: List[AnyValue], state: State, instance: ACrdtInstance): LazyList[AnyValue] = {
    val state2: State = makeState(state, instance)

    typeCheckState(instance, state2)

    instance.queryDefinitions().find(q => q.name.name == name) match {
      case Some(query) =>
        // evaluate query
        val prog = InProgram(
          name = "program",
          source = null,
          procedures = List[InProcedure](),
          types = instance.additionalDataTypesRec,
          axioms = List[InAxiomDecl](),
          invariants = List[InInvariantDecl](),
          programCrdt = instance
        )
        val interpreter = new Interpreter(prog, RunArgs()) {
          override def customTypeDomain(t: String): LazyList[AnyValue] =  t match {
            case "String" => LazyList("x", "y", "z", "a", "b", "c", "d").map(AnyValue)
            case "Int" => LazyList(1, 2, 3, 4, 101, 102, 103, 104).map(AnyValue)
          }
        }
        val localState = LocalState(
          currentInvoc = None,
          varValues = Map(),
          todo = List(),
          waitingFor = WaitForBegin(),
          currentTransaction = None,
          visibleCalls = state2.calls.keySet // todo: how to handle visible states
        )
        interpreter.evaluateQueryDeclLazyList(query, args, localState, state2)(interpreter.defaultAnyValueCreator)
      case None =>
        throw new RuntimeException(s"Query $name not defined for $instance. available queries: ${instance.queryDefinitions().map(_.name)}")
    }
  }

  private def makeState(state: State, instance: ACrdtInstance) = {
    state
  }

  def op(callId: Int, name: String, args: Any*): Op =
    Op(CallId(callId), name, args.toList.map(AnyValue))

  def o(name: String, args: Any*) = {
    DataTypeValue(name, args.toList.map(AnyValue))
  }

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
          operation = DataTypeValue("Op", List(AnyValue(DataTypeValue(c.name, c.args)))),
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
