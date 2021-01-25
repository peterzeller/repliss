package repliss

import crdtver.RunArgs
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, DataTypeCase, InAxiomDecl, InInvariantDecl, InProcedure, InProgram, InTypeExpr, IntType, SimpleType}
import crdtver.language.crdts.{ACrdtInstance, CounterCrdt, CrdtTypeDefinition}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, LocalState, SnapshotTime, State, TransactionId, WaitForNothing}
import crdtver.utils.LazyListUtils
import crdtver.utils.LazyListUtils.LazyListExtensions
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class CrdtQueryPropTests extends AnyFunSuite {


  test("Counter") {
    val genOp = for (i <- LazyList(1, -1)) yield DataTypeValue("Increment", List(AnyValue(i)))
    testProps(new CounterCrdt().instantiate(List(), List()), genOp)
  }


  def genDeps(ops: List[DataTypeValue]): LazyList[List[CallInfo]] = {
    val tx = TransactionId(0)
    val invoc = InvocationId(0)

    def rec(ops: List[DataTypeValue], i: Int, deps: Set[CallId]): LazyList[List[CallInfo]] = ops match {
      case List() => LazyList(List())
      case op :: rest =>
        val id = CallId(i)

        for {
          deps <- LazyListUtils.allSubsets(deps)
          ci = CallInfo(id, DataTypeValue("Op", List(AnyValue(op))), SnapshotTime(deps), tx, invoc)
          rest <- rec(rest, i+1, deps + id)
        } yield ci :: rest
    }

    rec(ops, 0, Set())
  }

  def testProps(crdt: ACrdtInstance, genOps: LazyList[DataTypeValue]): Unit = {

    def genType(typ: TypedAst.InTypeExpr): LazyList[AnyValue] = {
      typ match {
        case BoolType() =>
          LazyList(AnyValue(false), AnyValue(true))
        case IntType() =>
          (0 to 2).map(AnyValue).to(LazyList)
        case SimpleType(name, typeArgs) =>
          val dt = crdt.additionalDataTypesRec.find(_.name.name == name).get.instantiate(typeArgs)
          genDt(dt).map(AnyValue)
      }

    }

    def genDt(opDt: TypedAst.InTypeDecl): LazyList[DataTypeValue] = {
      for {
        c <- opDt.dataTypeCases.to(LazyList)
        params = LazyListUtils.allCombinations(c.params.map(p => genType(p.typ)))
        p <- params
      } yield DataTypeValue(c.name.name, p)
    }

    def genHistories(opDt: TypedAst.InTypeDecl): LazyList[State] = {
      for {
        ops <- LazyListUtils.listsUpToSize(4, genDt(opDt))
        deps <- genDeps(ops)
      } yield State(
        calls = deps.map(x => (x.id, x)).toMap
      )
    }

    def genHistories2: LazyList[State] = {
      for {
        ops <- LazyListUtils.listsUpToSize(4, genOps)
        deps <- genDeps(ops)
      } yield State(
        calls = deps.map(x => (x.id, x)).toMap
      )
    }


    val opType = crdt.operationType.asInstanceOf[SimpleType]
    val opDt: TypedAst.InTypeDecl = crdt.additionalDataTypesRec.find(_.name.name == opType.name)
      .map(_.instantiate(opType.typeArgs))
      .getOrElse(throw new RuntimeException(s"Could not find opType $opType."))


    val prog = InProgram(
      name = "program",
      source = null,
      procedures = List[InProcedure](),
      types = crdt.additionalDataTypesRec,
      axioms = List[InAxiomDecl](),
      invariants = List[InInvariantDecl](),
      programCrdt = crdt
    )
    val interpreter = new Interpreter(prog, RunArgs())


    val histories = genHistories2

    for {
      history <- histories
      qry <- genType(crdt.queryType)
    } {
      CrdtQueryTests.typeCheckState(crdt, history)

      val q = qry.value.asInstanceOf[DataTypeValue]
      val qryResult1 = crdt.evaluateQuery(q.operationName, q.args, history, interpreter).get
      val qrySpec = crdt.queryDefinitions().find(_.name.name == q.operationName).get

      val ls = LocalState(
        currentInvoc = None,
        varValues = Map(),
        todo = List(),
        waitingFor = WaitForNothing(),
        currentTransaction = None,
        visibleCalls = history.calls.keySet
      )

      val qryResult2 = interpreter.evaluateQueryDecl(qrySpec, q.args, ls, history)(interpreter.defaultAnyValueCreator)
      assert(qryResult1 == qryResult2)
    }




  }

}
