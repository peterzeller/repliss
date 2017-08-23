package repliss

import crdtver.Repliss
import crdtver.Repliss._
import crdtver.language.InputAst
import crdtver.utils.Helper
import org.scalatest._

/**
  * run with
  * sbt
  * testOnly *CrdtTests
  */
class CrdtTests extends FlatSpec with Matchers {


  def checkResource(name: String): Result[InputAst.InProgram] = {
    val input = Helper.getResource(name)
    val result: Result[InputAst.InProgram] = Repliss.parseInput(name, input)
    return result
  }


  "verifier" should "verify userbase example" in {

    val res = checkResource("/examples/task2.rpls")
    res match {
      case NormalResult(prog) =>
        // ok
      case ErrorResult(errors) =>
        fail(errors.head.toString)
    }


  }

}
