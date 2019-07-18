package repliss

import crdtver.testing.TreeWalker
import crdtver.utils.LazyListUtils
import crdtver.utils.LazyListUtils.LazyListExtensions
import org.scalatest.{FunSuite, Matchers}

/**
  * Tests for the random test generator
  */
class LazyListUtilsTest extends FunSuite with Matchers {



  test("allCombinations example") {
    val list = List(
      LazyList("a", "b"),
      LazyList("1", "2", "3"),
      LazyList("x", "y"),
    )

    val all = LazyListUtils.allCombinations(list)

    assert(all == LazyList(
      List("a", "1", "x"),
      List("b", "1", "x"),
      List("a", "2", "x"),
      List("b", "2", "x"),
      List("a", "3", "x"),
      List("b", "3", "x"),
      List("a", "1", "y"),
      List("b", "1", "y"),
      List("a", "2", "y"),
      List("b", "2", "y"),
      List("a", "3", "y"),
      List("b", "3", "y")
    ))


  }


}
