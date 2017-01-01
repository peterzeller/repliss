package repliss

import crdtver.Repliss
import crdtver.Repliss.Valid
import org.scalatest._

class ExampleTests extends FlatSpec with Matchers{


  "verifier" should "verify userbase example" in {

    val res = Repliss.check("examples/userbase.scala")
    res.hasErrors() should be (false)

    assert(res.get().forall(r => r.res == Valid()))

  }


}
