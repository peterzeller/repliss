package repliss

import crdtver.Repliss._
import crdtver.{Repliss, RunArgs}
import crdtver.utils.Helper
import org.scalatest._
import org.scalatest.tagobjects.Slow

class ExampleTests extends FlatSpec with Matchers {


  def checkResource(name: String): Result[ReplissResult] = {
    val input = Helper.getResource(name)
    Repliss.checkInput(input, name, runArgs = RunArgs())
  }


  "verifier" should "verify userbase example" taggedAs(Slow) in {

    val res = checkResource("/examples/userbase.rpls")

    println(s"why3results = ${res.get().why3Results}")

    assert(!res.get().hasCounterexample)
    assert(res.get().isVerified)
  }

  it should "fail to verify userbase_fail1" taggedAs(Slow) in {
    val res = checkResource("/examples/userbase_fail1.rpls")

    assert(res.get().hasCounterexample)
    assert(!res.get().isVerified)
  }

  it should "fail to verify userbase_fail2" taggedAs(Slow) in {
    val res = checkResource("/examples/userbase_fail2.rpls")

    assert(res.get().hasCounterexample)
    assert(!res.get().isVerified)
  }


  it should "verify friends example" taggedAs(Slow) in {

    val res = checkResource("/examples/friends.rpls")

    println(s"why3results = ${res.get().why3Results}")

    assert(!res.get().hasCounterexample)
    assert(res.get().isVerified)

  }

  // not working at the moment, must be done manually
  //  it should "verify friends2 example" in {
  //
  //    val res = checkResource("/examples/friends2.rpls")
  //    res.hasErrors() should be (false)
  //
  //    assert(res.get().forall(r => r.res == Valid()))
  //
  //  }
}
