package repliss

import crdtver.testing.{Interpreter, TreeWalker}
import crdtver.testing.Interpreter.{AnyValue, CallAction, InvariantCheck, InvariantViolationException, InvocationId, LocalAction, NewId, Return, StartTransaction, TransactionId}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.{FunSuite, Matchers}

/**
  * Tests for the random test generator
  */
class TreeWalkerTests extends FunSuite with Matchers {




  test("tree walker example") {
    val paths = TreeWalker.walkTree("")(s => Stream(s + "a", s + "b"))
    for (p <- paths.take(100))
      println(s"'$p'")


  }


}
