package repliss

import crdtver.testing.{Interpreter, TreeWalker}
import crdtver.testing.Interpreter.{AnyValue, CallAction, InvariantCheck, InvariantViolationException, InvocationId, LocalAction, NewId, Return, StartTransaction, TransactionId}
import crdtver.utils.LazyListUtils.LazyListExtensions
import crdtver.utils.{Helper, LazyListUtils}
import crdtver.{Repliss, RunArgs}
import org.scalatest.{FunSuite, Matchers}

/**
  * Tests for the random test generator
  */
class TreeWalkerTests extends FunSuite with Matchers {



  test("tree walker example") {
    val paths = TreeWalker.walkTree[String]("", s => LazyList(s + "a", s + "b"))
    val pathsL = paths.take(100)
    assert(pathsL contains List())
    assert(pathsL contains List("a", "ab", "aba"))
    assert(pathsL contains List("a", "aa", "aab"))
    assert(pathsL contains List("b", "ba", "bab", "babb", "babba"))
  }

  test("combine test") {
    val combined = LazyListUtils.combine("a", LazyList(LazyList("b", "c"), LazyList("x", "y")))
    assert(combined ==
      LazyList(
        LazyList("a", "b", "c"),
        LazyList("a", "x", "y"),
        LazyList("a")
      ))

  }

  test("breadthfirst test") {
    val l = for {
      x <- new LazyListExtensions(LazyList(1,2,3)).breadthFirst
      y <- LazyList("a", "b", "c")
    } yield (x,y)

    assert(l == LazyList((1,"a"), (2,"a"), (3,"a"), (1,"b"), (2,"b"), (3,"b"), (1,"c"), (2,"c"), (3,"c")))

  }

//  test("paths test") {
////    val paths = TreeWalker.paths[String]("", s => LazyList.iterate(0)(_+1).map(i => s"$s-$i"))
//
//    val paths = TreeWalker.paths[String]("0", s => {
//      if (s.length > 10) LazyList()
//      else LazyList.iterate(0)(_+1).map(i => s"$s-$i")
//    })
//
//    println("paths calculated")
//
//    for (p <- paths.take(20)) {
//      println(p.take(3).toList)
//    }
//
//
//  }

//  test("tree walker2 example") {
//    val t = TreeWalker.tree[String]("0", s => LazyList.iterate(0)(_+1).map(i => s"$s-$i"))
//    println(s"tree =\n${t.print()}")
//
//    val elems = TreeWalker.walkTree4[String](t, 4)
//    for (p <- elems.take(100))
//      println(s"walkTree2 '$p'")
//
//
//  }


}
