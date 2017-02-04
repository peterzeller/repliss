package repliss

import java.io.InputStream

import crdtver.{Helper, Repliss}
import crdtver.Repliss.{ReplissResult, Result, Valid, Why3Result}
import org.scalatest._

class ExampleTests extends FlatSpec with Matchers with ParallelTestExecution{

  def checkResource(name: String): Result[ReplissResult] = {
//    val stream : InputStream = getClass.getResourceAsStream(name)
//
//
//    val input = scala.io.Source.fromInputStream(stream).mkString

    val input = Helper.getResource(name)
    Repliss.checkInput(input, name)
  }


  "verifier" should "verify userbase example" in {

    val res = checkResource("/examples/userbase.rpls")
    res.hasErrors() should be (false)

    assert(res.get().isValid)

  }

  it should "fail to verify userbase_fail1" in {
    val res = checkResource("/examples/userbase_fail1.rpls")
    res.hasErrors() should be (false)

    assert(res.get().hasCounterexample)
    assert(!res.get().isVerified)
  }

  it should "fail to verify userbase_fail2" in {
    val res = checkResource("/examples/userbase_fail2.rpls")
    res.hasErrors() should be (false)

    assert(res.get().hasCounterexample)
    assert(!res.get().isVerified)
  }

  it should "verify friends example" in {

    val res = checkResource("/examples/friends.rpls")
    res.hasErrors() should be (false)

    assert(res.get().isValid)

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
