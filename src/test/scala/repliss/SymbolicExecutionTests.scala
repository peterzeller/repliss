package repliss

import crdtver.Repliss.{ReplissResult, Result, SymbolicCheck}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.tagobjects.Slow
import org.scalatest.{FlatSpec, Matchers}

class SymbolicExecutionTests extends FlatSpec with Matchers {

  //  def checkResource(name: String): Result[ReplissResult] = {
  //    val input = Helper.getResource(name)
  //    Repliss.checkInput(input, name, runArgs = RunArgs())
  //  }

  private def checkString(name: String, input: String): Result[ReplissResult] = {
    Repliss.checkInput(input, name, runArgs = RunArgs(), checks = List(SymbolicCheck()))
  }

  private def checkResource(name: String): Result[ReplissResult] = {
    val input = Helper.getResource(name)
    checkString(name, input)
  }

  "symbolic execution" should "find error in max" taggedAs (Slow) in {

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

    println(s"symbolicCounterexample = ${res.get().symbolicCounterexample}")

    assert(res.get().hasSymbolicCounterexample)
  }


  "symbolic execution" should "verify userbase example" taggedAs (Slow) in {

    val res = checkResource("/examples/userbase.rpls")

    println(s"symbolicCounterexample = ${res.get().symbolicCounterexample}")

    assert(res.get().hasSymbolicCounterexample)
  }

  "symbolic execution" should "fail to verify userbase_fail1" taggedAs (Slow) in {
    val res = checkResource("/examples/userbase_fail1.rpls")

    println(s"symbolicCounterexample = ${res.get().symbolicCounterexample}")

    assert(res.get().hasSymbolicCounterexample)
  }

}
