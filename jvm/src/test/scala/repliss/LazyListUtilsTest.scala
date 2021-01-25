package repliss

import crdtver.utils.LazyListUtils
import crdtver.utils.LazyListUtils.LazyListExtensions
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Tests for the random test generator
 */
class LazyListUtilsTest extends AnyFunSuite with Matchers {

  test("breadthFirst example") {

    val list =
      for {
        x <- LazyList("a", "b").breadthFirst
        y <- LazyList(1,2,3)
      } yield (x, y)

    assert(list.toList == List("a" -> 1, "b" -> 1, "a" -> 2, "b" -> 2, "a" -> 3, "b" -> 3))
  }

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

  test("listsWithSize") {
    assert(LazyListUtils.listsWithSize(3, LazyList(1, 2, 3)).toList == List(
      List(1, 1, 1), List(1, 1, 2), List(1, 1, 3), List(1, 2, 1), List(1, 2, 2), List(1, 2, 3), List(1, 3, 1), List(1, 3, 2), List(1, 3, 3),
      List(2, 1, 1), List(2, 1, 2), List(2, 1, 3), List(2, 2, 1), List(2, 2, 2), List(2, 2, 3), List(2, 3, 1), List(2, 3, 2), List(2, 3, 3),
      List(3, 1, 1), List(3, 1, 2), List(3, 1, 3), List(3, 2, 1), List(3, 2, 2), List(3, 2, 3), List(3, 3, 1), List(3, 3, 2), List(3, 3, 3)))
  }

  test("listsUpToSize") {
    assert(LazyListUtils.listsUpToSize(2, LazyList(1, 2, 3)).toList == List(
      List(),
      List(1), List(2), List(3),
      List(1, 1), List(1, 2), List(1, 3), List(2, 1), List(2, 2), List(2, 3), List(3, 1), List(3, 2), List(3, 3)))
  }

  test("allSubsets") {
    assert(LazyListUtils.allSubsets(List(1,2,3)).toList ==
      List(Set(), Set(1), Set(2), Set(1, 2), Set(3), Set(1, 3), Set(2, 3), Set(1, 2, 3)))
  }

}
