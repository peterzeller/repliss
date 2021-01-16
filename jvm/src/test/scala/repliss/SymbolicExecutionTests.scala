package repliss

import crdtver.Repliss.{ReplissResult, Result, SymbolicCheck}
import crdtver.symbolic.Cvc4Proxy
import crdtver.symbolic.smt.{Cvc4Solver, Solver, Z3Solver}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import edu.nyu.acsys.CVC4.{Datatype, DatatypeConstructor, DatatypeType, Expr, ExprManager, Kind, Rational, SExpr, SWIGTYPE_p_CVC4__Model, SmtEngineI, Type, vectorType}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/**
  * Tests for the verifier based on symbolic execution.
  */
class SymbolicExecutionTests extends AnyFunSuite with Matchers {

  //  def checkResource(name: String): Result[ReplissResult] = {
  //    val input = Helper.getResource(name)
  //    Repliss.checkInput(input, name, runArgs = RunArgs())
  //  }

  private def checkString(name: String, input: String, runArgs: RunArgs = RunArgs()): ReplissResult = {
    val res = Repliss.checkInput(input, name, runArgs = runArgs, checks = List(SymbolicCheck()))
    res match {
      case Repliss.NormalResult(rr) =>
        Repliss.printSymbolicExecutionResult(rr, name, System.out)
        rr
      case Repliss.ErrorResult(errors) =>
        throw new RuntimeException(errors.map(_.toString).mkString("\n"))
    }
  }

  private def checkResource(name: String, runArgs: RunArgs = RunArgs()): ReplissResult = {
    val input = Helper.getResource(name)
    checkString(name, input, runArgs)
  }

  test("find error in max") {

    val res = checkString("numbers",
      """
        |def max(x: Int, y: Int, z: Int): Int
        |  var m: Int
        |  m = 42
        |  if x > y && x > z
        |   m = x
        |  else if y > x && y > z
        |   m = y
        |  else
        |   m = z
        |
        |  assert m >= x
        |  assert m >= y
        |  assert m >= z
        |  return m
        |
        |
      """.stripMargin)

    assert(res.hasSymbolicCounterexample)
  }


  test("find error with generic datatypes") {

      val res = checkString("either",
        """
          |type Either[A, B] = Left(a: A) | Right(b: B)
          |
          |def minmax(x: Int, y: Either[Int, Int]): Int
          |  var m: Int
          |  m = x
          |  y match
          |    case Left(z) =>
          |       if z < x
          |          m = z
          |    case Right(z) =>
          |       if z > x
          |          m = z
          |
          |  assert (forall z :: y == Left(z) ==> m <= x && m <= z)
          |  assert (forall z :: y == Right(z) ==> m >= x && m >= z)
          |  return m
          |
          |
        """.stripMargin,
        // use z3 solver, since cvc4 cannot solve this
      runArgs = RunArgs(inferShapeInvariants = false, solver = Solver.parseSolver("I(z3|cvc4|cvc4f)")))

      assert(!res.hasSymbolicCounterexample)
    }


  test("verify userbase example", Slow) {
    val res = checkResource("/examples/verified/userbase.rpls")
    assert(!res.hasSymbolicCounterexample)
  }

  test("verify userbase2 example", Slow) {
    val res = checkResource("/examples/verified/userbase2.rpls")
    assert(!res.hasSymbolicCounterexample)
  }

  test("fail to verify broken userbase example", Slow) {
    // disable Z3 because it crashes
    val res = checkResource("/examples/buggy/userbase_fail1.rpls", RunArgs(solver = Solver.parseSolver("cvc4|cvc4f")))
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
