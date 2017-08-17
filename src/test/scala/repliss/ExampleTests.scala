package repliss

import crdtver.Repliss._
import crdtver.{Helper, Repliss}
import org.scalatest._

class ExampleTests extends FlatSpec with Matchers {


  def checkResource(name: String): Result[ReplissResult] = {
    val input = Helper.getResource(name)
    Repliss.checkInput(input, name)
  }


  "verifier" should "verify userbase example" in {

    val res = checkResource("/examples/userbase.rpls")

    println(s"why3results = ${res.get().why3Results}")

    assert(!res.get().hasCounterexample)
    assert(res.get().isVerified)
  }

  it should "fail to verify userbase_fail1" in {
    val res = checkResource("/examples/userbase_fail1.rpls")

    assert(res.get().hasCounterexample)
    assert(!res.get().isVerified)
  }

  it should "fail to verify userbase_fail2" in {
    val res = checkResource("/examples/userbase_fail2.rpls")

    assert(res.get().hasCounterexample)
    assert(!res.get().isVerified)
  }


  it should "verify friends example" in {

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
