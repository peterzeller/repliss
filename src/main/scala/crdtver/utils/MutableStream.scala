package crdtver.utils

import java.util.concurrent.LinkedBlockingQueue


class MutableStream[T] {
  // None for end
  private val unprocessed = new LinkedBlockingQueue[Option[T]]

  //noinspection VarCouldBeVal
  private var default: T = _

  private val streamH: Stream[T] = default #:: makeStream()

  def stream: Stream[T] = streamH.tail

  def push(elem: T): Unit = {
    unprocessed.put(Some(elem))
  }

  def complete(): Unit = {
    unprocessed.put(None)
  }

  private def makeStream(): Stream[T] = {
      unprocessed.take() match {
        case Some(x) =>
          x #:: makeStream()
        case None =>
          Stream.empty
      }
  }

}

object MutableStreamTest {
  def test(): Unit = {
    val m = new MutableStream[Int]
    ConcurrencyUtils.spawn[Unit](() => {
      for (i <- 1 to 10) {
        Thread.sleep(100)
        m.push(i)
      }
      m.complete()
    })

    ConcurrencyUtils.spawn[Unit](() => {
      for (s <- m.stream) {
        Thread.sleep(200)
        println(s"got $s")
      }
    })
    println(s"list = ${m.stream.toList}")
  }
}