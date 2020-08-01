package repliss

import crdtver.Repliss.{ReplissResult, Result, SymbolicCheck}
import crdtver.symbolic.Cvc4Proxy
import crdtver.symbolic.smt.Cvc4Solver
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import edu.nyu.acsys.CVC4.{Datatype, DatatypeConstructor, DatatypeType, Expr, ExprManager, Kind, Rational, SExpr, SWIGTYPE_p_CVC4__Model, SmtEngineI, Type, vectorType}
import org.scalatest.tagobjects.Slow
import org.scalatest.{FlatSpec, FunSuite, Matchers}

/**
  * Tests for the verifier based on symbolic execution.
  */
class SymbolicExecutionTests extends FunSuite with Matchers {

  //  def checkResource(name: String): Result[ReplissResult] = {
  //    val input = Helper.getResource(name)
  //    Repliss.checkInput(input, name, runArgs = RunArgs())
  //  }

  private def checkString(name: String, input: String): ReplissResult = {
    val res = Repliss.checkInput(input, name, runArgs = RunArgs(), checks = List(SymbolicCheck()))
    res match {
      case Repliss.NormalResult(rr) =>
        Repliss.printSymbolicExecutionResult(rr, name, new Object())
        rr
      case Repliss.ErrorResult(errors) =>
        throw new RuntimeException(errors.map(_.toString).mkString("\n"))
    }
  }

  private def checkResource(name: String): ReplissResult = {
    val input = Helper.getResource(name)
    checkString(name, input)
  }

  test("find error in max") {

    val res = checkString("numbers",
      """
        |def max(x: int, y: int, z: int): int {
        |  var m: int
        |  m = 42
        |  if (x > y && x > z) {
        |   m = x
        |  } else if (y > x && y > z) {
        |   m = y
        |  } else {
        |   m = z
        |  }
        |  assert m >= x
        |  assert m >= y
        |  assert m >= z
        |  return m
        |}
        |
      """.stripMargin)

    assert(res.hasSymbolicCounterexample)
  }

  test("cvc4 generics") {
    Cvc4Solver.loadLibrary()
    val em = new ExprManager
    val smt: SmtEngineI = Cvc4Proxy.smtEngine(em)
    val types = new vectorType()
    val t: Type = em.mkSort("T")
    types.add(t)
    val dt = Cvc4Proxy.Datatype("option", types)
    val cNone = new DatatypeConstructor("None")
    dt.addConstructor(cNone)
    val cSome = new DatatypeConstructor("Some")
    cSome.addArg("elem", t)
    dt.addConstructor(cSome)

    val dt2 = em.mkDatatypeType(dt)

//    val l: Expr = em.mkExpr(Kind.APPLY_CONSTRUCTOR, dt2.getConstructor("Some"), em.mkConst(new Rational("42")))
    val dt2Int: DatatypeType = dt2.instantiate(Cvc4Proxy.toVectorType(List(em.integerType())))
    println(s"dt2Int = $dt2Int // ${dt2Int.isParametric} // {dt2Int.isInstantiated}")
    val noneConstructor: Expr = dt2Int.getConstructor("None")
    println(s"noneConstructor = $noneConstructor")
    println(s"noneConstructor.t =  ${noneConstructor.getType}")
    val l: Expr = em.mkExpr(Kind.APPLY_CONSTRUCTOR, noneConstructor)
    val x = em.mkVar("x", em.integerType())
    val r: Expr = em.mkExpr(Kind.APPLY_CONSTRUCTOR, dt2.getConstructor("Some"), x)
    val eq = em.mkExpr(Kind.EQUAL, l, r)


    smt.setOption("produce-models", new SExpr(true))
    smt.setOption("finite-model-find", new SExpr(true))
    smt.setOption("sets-ext", new SExpr(true))
    smt.assertFormula(eq)
    val res = smt.checkSat()
    println(s"res = $res")
    val model: SWIGTYPE_p_CVC4__Model = smt.getModel
    println(s"model = $model")
    val xVal = smt.getValue(x)
    println(s"x = $xVal")


  }

  test("find error with generic datatypes") {

      val res = checkString("either",
        """
          |type Either[A, B] = Left(a: A) | Right(b: B)
          |
          |def minmax(x: Int, y: Either[Int, Int]): Int {
          |  var m: Int
          |  m = x
          |  y match {
          |   case Left(z) =>
          |      if (z < x)
          |         m = z
          |   case Right(z) =>
          |      if (z > x)
          |         m = z
          |  }
          |  assert (forall z :: y == Left(z) ==> m <= x && m <= z)
          |  assert (forall z :: y == Right(z) ==> m >= x && m >= z)
          |  return m
          |}
          |
        """.stripMargin)

      assert(res.hasSymbolicCounterexample)
    }


  test("verify userbase example", Slow) {
    val res = checkResource("/examples/verified/userbase.rpls")
    assert(!res.hasSymbolicCounterexample)
  }

  test("fail to verify broken userbase example", Slow) {
    val res = checkResource("/examples/failsToVerify/userbase_fail1.rpls")
    assert(res.hasSymbolicCounterexample)
  }


  test("verify chatapp example", Slow) {
    val res = checkResource("/examples/verified/chatapp.rpls")
    assert(!res.hasSymbolicCounterexample)
  }

  test("verify chatapp_si example", Slow) {
    val res = checkResource("/examples/verified/chatapp_si.rpls")
    assert(!res.hasSymbolicCounterexample)
  }
}
