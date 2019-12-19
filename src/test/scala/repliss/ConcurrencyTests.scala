package repliss

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import crdtver.utils.ConcurrencyUtils
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration.Duration

/**
  * Tests for the random test generator
  */
class ConcurrencyTests extends FunSuite with Matchers {

  import ConcurrencyUtils._

  import scala.concurrent.ExecutionContext.Implicits.global

  test("wait for all") {
    val allT: Task[List[Int]] = ConcurrencyUtils.all(
      for (i <- (1 to 100).toList)
        yield spawn(() => {
          Thread.sleep(100)
          i
        }))
    val all = allT.await(Duration(200, TimeUnit.MILLISECONDS))
    assert(all == (1 to 100).toList)
  }

  test("race") {
    val n = 20
    val locks = Array.fill(n+1)(new Object)
    val results: LazyList[Int] = ConcurrencyUtils.race(
      for (i <- (1 to n).toList)
        yield spawn(() => {
          if (i < n) {
            locks(i + 1).synchronized {
              locks(i + 1).wait(5000)
            }
          }
          Thread.sleep(10)
          locks(i).synchronized {
            locks(i).notifyAll()
          }
          i
        }))
    assert(results.toList == (1 to n).reverse.toList)
  }

  test("race abort") {
    val tasks: List[Task[Int]] = List(
      spawn(() => {
        try {
          while (true) {
            Thread.sleep(100)
            println("waiting ...")
          }
          99
        } catch {
          case _: InterruptedException =>
            4711
        }
      }),
        spawn(() => {
        42
      })
    )
    val results: LazyList[Int] = ConcurrencyUtils.race(tasks)
    assert(results.head == 42)
    tasks.foreach(_.cancel())
    assert(results.toList == List(42, 4711))
  }



}
