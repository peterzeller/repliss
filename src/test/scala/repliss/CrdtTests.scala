package repliss

import crdtver.Repliss
import crdtver.Repliss._
import crdtver.language.crdts.CrdtContext
import crdtver.language.{InputAst, Typer}
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

    val res = checkResource("/examples/wip/task2.rpls")
    res match {
      case NormalResult(prog) =>
        // ok
        println(prog)
        val nameContext = new CrdtContext
        val typer = new Typer(nameContext)
        val result = typer.checkProgram(prog)
        println(result)
        result match {
          case NormalResult(typedprog) =>
            println(typedprog)
          case ErrorResult(errors) =>
            fail(errors.head.toString)
        }
      case ErrorResult(errors) =>
        fail(errors.head.toString)
    }
  }
}
