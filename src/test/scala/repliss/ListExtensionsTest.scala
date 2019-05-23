package repliss

import crdtver.utils.ListExtensions
import org.scalatest.FunSuite

class ListExtensionsTest extends FunSuite {

  test("splitOffOne") {
    val input = List(1, 2, 3, 4)
    assert(ListExtensions.splitOffOne(input) == List(
      (List(), 1, List(2, 3, 4)),
      (List(1), 2, List(3, 4)),
      (List(1, 2), 3, List(4)),
      (List(1, 2, 3), 4, List()),
    ))

  }

}
