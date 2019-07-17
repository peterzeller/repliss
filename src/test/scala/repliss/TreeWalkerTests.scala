package repliss

import crdtver.testing.{Interpreter, LazyList, TreeWalker}
import crdtver.testing.Interpreter.{AnyValue, CallAction, InvariantCheck, InvariantViolationException, InvocationId, LocalAction, NewId, Return, StartTransaction, TransactionId}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.{FunSuite, Matchers}

/**
  * Tests for the random test generator
  */
class TreeWalkerTests extends FunSuite with Matchers {

  test("generate take") {
    def inc(x: Int): Int ={
      val r = x + 1
      println(s"generate $r")
      r
    }

    val l = LazyList.iterate(0)(inc)
    for (p <- l.take(10)) {
      println(p)
    }

  }


//  test("tree walker example") {
//    val paths = TreeWalker.walkTree[String]("", s => LazyList(s + "a", s + "b"))
//    for (p <- paths.take(100))
//      println(s"'$p'")
//  }

//  test("paths test") {
//    val paths = TreeWalker.paths[String]("", s => LazyList.iterate(0)(_+1).map(i => s"$s-$i"))
//    for (p <- paths.take(10))
//      println(p.take(3).toList)
//
//
//  }

//  test("tree walker2 example") {
//    val paths = TreeWalker.walkTree2[String]("", 2, s => LazyList.iterate(0)(_+1).map(i => s"$s-$i"))
//    for (p <- paths.take(100))
//      println(s"'$p'")
//
//
//  }


}
