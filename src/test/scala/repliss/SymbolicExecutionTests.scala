package repliss

import crdtver.Repliss.{ReplissResult, Result, SymbolicCheck}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
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


  test("verify userbase example", Slow) {

    val res = checkResource("/examples/verified/userbase.rpls")

    assert(!res.hasSymbolicCounterexample)
  }

  test("fail to verify broken userbase example", Slow) {
    val res = checkResource("/examples/failsToVerify/userbase_fail1.rpls")

    assert(res.hasSymbolicCounterexample)
  }

}
