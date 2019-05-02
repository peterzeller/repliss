package repliss

import crdtver.Repliss.{Quickcheck, ReplissResult, Result, SymbolicCheck}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.tagobjects.Slow
import org.scalatest.{FlatSpec, FunSuite, Matchers}

/**
  * Tests for the random test generator
  */
class QuickcheckTests extends FunSuite with Matchers {

  //  def checkResource(name: String): Result[ReplissResult] = {
  //    val input = Helper.getResource(name)
  //    Repliss.checkInput(input, name, runArgs = RunArgs())
  //  }

  private def checkString(name: String, input: String): ReplissResult = {
    val res = Repliss.checkInput(input, name, runArgs = RunArgs(), checks = List(Quickcheck()))
    res match {
      case Repliss.NormalResult(rr) =>
        Repliss.printTestingResult(rr, name, new Object())
        rr
      case Repliss.ErrorResult(errors) =>
        throw new RuntimeException(errors.map(_.toString).mkString("\n"))
    }
  }

  private def checkResource(name: String): ReplissResult = {
    val input = Helper.getResource(name)
    checkString(name, input)
  }


  test("no counterexample for userbase", Slow) {

    val res = checkResource("/examples/verified/userbase.rpls")

    assert(!res.hasCounterexample)
  }

  test("userbase_fail1 counterexample", Slow) {
    val res = checkResource("/examples/failsToVerify/userbase_fail1.rpls")

    assert(res.hasCounterexample)
  }

}
