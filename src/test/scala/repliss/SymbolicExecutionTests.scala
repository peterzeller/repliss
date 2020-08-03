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
        |def max(x: Int, y: Int, z: Int): Int {
        |  var m: Int
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

      assert(!res.hasSymbolicCounterexample)
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
