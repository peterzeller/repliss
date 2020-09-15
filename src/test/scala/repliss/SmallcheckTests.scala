package repliss

import crdtver.Repliss.{Quickcheck, ReplissResult, SmallCheck}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.concurrent.duration.DurationInt

/**
  * Tests for the random test generator
  */
class SmallcheckTests extends AnyFunSuite with Matchers {

  //  def checkResource(name: String): Result[ReplissResult] = {
  //    val input = Helper.getResource(name)
  //    Repliss.checkInput(input, name, runArgs = RunArgs())
  //  }

  private def checkString(name: String, input: String, runArgs: RunArgs): ReplissResult = {
    val res = Repliss.checkInput(input, name, runArgs = runArgs, checks = List(SmallCheck()))
    res match {
      case Repliss.NormalResult(rr) =>
        Repliss.printTestingResultSmallCheck(rr, name, System.out)
        rr
      case Repliss.ErrorResult(errors) =>
        throw new RuntimeException(errors.map(_.toString).mkString("\n"))
    }
  }

  private def checkResource(name: String, runArgs: RunArgs): ReplissResult = {
    val input = Helper.getResource(name)
    checkString(name, input, runArgs)
  }


  test("no counterexample for userbase", Slow) {

    val res = checkResource("/examples/verified/userbase.rpls", RunArgs(timeout = 2.minutes))

    assert(!res.hasSmallCheckCounterexample)
  }

  test("userbase_fail1 counterexample", Slow) {
    val res = checkResource("/examples/buggy/userbase_fail1.rpls", RunArgs(timeout = 10.minutes))

    assert(res.hasSmallCheckCounterexample)
  }

}
