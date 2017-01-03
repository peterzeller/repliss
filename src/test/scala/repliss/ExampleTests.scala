package repliss

import crdtver.Repliss
import crdtver.Repliss.Valid
import org.scalatest._

class ExampleTests extends FlatSpec with Matchers with ParallelTestExecution{

  "verifier" should "verify userbase example" in {

    val res = Repliss.check("examples/userbase.scala")
    res.hasErrors() should be (false)

    assert(res.get().forall(r => r.res == Valid()))

  }

  it should "fail to verify userbase_fail1" in {
    val res = Repliss.check("examples/userbase_fail1.scala")
    res.hasErrors() should be (false)

    assert(!res.get().forall(r => r.res == Valid()))
  }

  it should "fail to verify userbase_fail2" in {
    val res = Repliss.check("examples/userbase_fail2.scala")
    res.hasErrors() should be (false)

    assert(!res.get().forall(r => r.res == Valid()))
  }

  it should "verify friends example" in {

    val res = Repliss.check("examples/friends.scala")
    res.hasErrors() should be (false)

    assert(res.get().forall(r => r.res == Valid()))

  }
}
