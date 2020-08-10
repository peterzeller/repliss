package repliss

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

  test("takeUntil example") {
    var count = 0

    def f: Int = {
      count += 1
      count
    }


    val list = LazyList.continually(f)
    assert(list.takeWhile(_ <= 3).toList == List(1, 2, 3))
    assert(list.takeUntil(_ >= 3).toList == List(1, 2, 3))
    assert(LazyList[Int]().takeWhile(_ >= 5).isEmpty)
    assert(LazyList[Int](42).takeWhile(_ >= 5) == LazyList(42))
    assert(list.takeUntil(_ >= 5).toList == List(1, 2, 3, 4, 5))
  }

  test("takeUntil is lazy") {
    val errList = LazyList.continually[Int]({
      throw new Exception("should not evaluate")
    })
    errList.takeUntil(_ >= 5)

    val list = LazyList(1, 2, 3, 4, 5) ++ errList
    assert(list.takeUntil(_ >= 5).toList == List(1, 2, 3, 4, 5))
  }

}
