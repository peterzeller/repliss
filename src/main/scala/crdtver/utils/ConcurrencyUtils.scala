package crdtver.utils

import scala.collection.immutable.Stream.Empty
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.language.higherKinds
import scala.util.{Failure, Success}
import scalaz.stream.Process

/**
  * Some concurrency helpers, I am sure there is a better library for this stuff
  */
object ConcurrencyUtils {
  def newThread[T](f: => T): Future[T] = {
    val p = Promise[T]
    val t = new Thread() {
      override def run(): Unit = {
        try {
          p.success(f)
        } catch {
          case e: Throwable => p.failure(e)
        }
      }
    }
    t.start()
    p.future
  }


  def futureToProcess[T, A[_]](fut: Future[T]): Process[A, T] = {
    val start: Option[Future[T]] = Some(fut)
    Process.unfold(start) {
      case Some(f) =>
        val result: T = Await.result(f, Duration.Inf)
        Some((result, None))
      case None =>
        None
    }
  }

  def streamToProcess[T, A[_]](stream: Stream[T]): Process[A, T] = {
    Process.unfold(stream) {
      case Empty =>
        None
      case str =>
        Some((str.head, str.tail))
    }
  }


  sealed abstract class Task[+T] {
    def future(): Future[T]

    def cancel(): Unit

    def await(duration: Duration = Duration.Inf) = {
      Await.result(future(), duration)
    }

    def map[S](f: T => S)(implicit executor: ExecutionContext): Task[S] = {
      val p = Promise[S]
      p.completeWith(future().map(f))
      new PromiseTask(p) {
        override def cancel(): Unit = {
          Task.this.cancel()
        }
      }
    }

    def flatMap[S](f: T => Task[S])(implicit executor: ExecutionContext): Task[S] = {
      val p = Promise[S]
      p.completeWith(future().map(f).flatMap(_.future()))
      new PromiseTask(p) {
        override def cancel(): Unit = {
          Task.this.cancel()
        }
      }
    }


    def race[S](other: Task[S])(implicit executor: ExecutionContext): Task[Either[T, S]] = {
      val p = Promise[Either[T, S]]
      p.tryCompleteWith(future().map(Left(_)))
      p.tryCompleteWith(other.future().map(Right(_)))

      new PromiseTask(p) {
        override def cancel(): Unit = {
          Task.this.cancel()
          other.cancel()
        }
      }
    }

    def join[S](other: Task[S])(implicit executor: ExecutionContext): Task[(T, S)] = {

      val p = Promise[(T, S)]
      p.completeWith(for (t <- future(); s <- other.future()) yield (t, s))


      new PromiseTask(p) {
        override def cancel(): Unit = {
          Task.this.cancel()
          other.cancel()
        }
      }
    }

  }

  private abstract class PromiseTask[T](p: Promise[T]) extends Task[T] {
    override def future(): Future[T] = p.future

  }

  private class ThreadTask[T](f: () => T) extends Task[T] {


    private val p = Promise[T]
    private val t = new Thread {
      override def run(): Unit = {
        try {
          val r = f()
          p.complete(Success(r))
        } catch {
          case e: Throwable =>
            p.complete(Failure(e))
        }
      }
    }
    t.setName("ThreadTask")
    t.setDaemon(true)
    t.start()

    def future(): Future[T] = p.future

    def cancel(): Unit = {
//      t.interrupt()
    }

  }

  def spawn[T](f: () => T): Task[T] = {
    new ThreadTask(f)
  }

  def all[T](tasks: List[Task[T]])(implicit executor: ExecutionContext): Task[List[T]] = {

    val p = Promise[List[T]]
    //    p.completeWith(for (f <- tasks; r <- f) yield r)
    val futures = tasks.map(_.future())
    val fut = Future.sequence(futures)
    p.completeWith(fut)

    new PromiseTask(p) {
      override def cancel(): Unit = {
        for (t <- tasks) {
          t.cancel()
        }
      }
    }
  }


}
