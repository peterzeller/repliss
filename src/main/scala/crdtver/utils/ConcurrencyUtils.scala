package crdtver.utils

import java.util.concurrent.{Callable, ExecutorService, Executors}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.language.higherKinds
import scala.util.{Failure, Success}

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

  def newThreadWithInterruptHandler[T](work: => T, onInterrupt: () => Unit, timeout: Duration = Duration.Inf): T = {
    val p = Promise[T]
    val t = new Thread() {
      override def run(): Unit = {
        try {
          p.success(work)
        } catch {
          case e: Throwable => p.failure(e)
        }
      }
    }
    t.start()
    try {
      Await.result(p.future, timeout)
    } finally {
      if (t.isAlive) {
        onInterrupt()
        t.interrupt()
      }
    }
  }


  sealed abstract class Task[+T] {
    def future(): Future[T]

    def cancel(): Unit

    def await(duration: Duration = Duration.Inf): T = {
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
      p.completeWith(future().map(Left(_)))
      p.completeWith(other.future().map(Right(_)))

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

  private class ThreadTask[T](work: () => T, onCancel: Thread => Unit) extends Task[T] {
    private val p = Promise[T]
    private val t = new Thread {
      override def run(): Unit = {
        try {
          val r = work()
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
      onCancel(t)
    }
  }

  def spawn[T](work: () => T, onCancel: Thread => Unit = _.interrupt()): Task[T] = {
    new ThreadTask(work, onCancel)
  }

  private class ETask[T](work: () => T, onCancel: Thread => Unit, ec: ExecutorService) extends Task[T] {
    private val fut = ec.submit(new Callable[T] {
      override def call(): T = work()
    })

    private val scalaFut = Future(fut.get())(ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor()))
    def future(): Future[T] = scalaFut

    def cancel(): Unit = {
      fut.cancel(true)
    }
  }

  def spawnE[T](work: () => T, onCancel: Thread => Unit = _.interrupt(), executor: ExecutorService): Task[T] = {
    new ETask(work, onCancel, executor)
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


  def race[T](tasks: List[Task[T]])(implicit executor: ExecutionContext): LazyList[T] = {

    val promises: List[Promise[T]] = for (t <- tasks) yield Promise[T]

    for (task <- tasks) {
      val es = Executors.newSingleThreadExecutor()
      task.future().onComplete(r => {
        // try to find the first promise to complete:
        promises.find(p => {
          try {
            p.complete(r)
            true
          } catch {
            case _: IllegalStateException =>
              false
          }
        })
      })(ExecutionContext.fromExecutorService(es))
    }

    LazyList.unfold(promises) {
      case List() => None
      case p :: ps =>
        val pRes = Await.result(p.future, Duration.Inf)
        Some((pRes, ps))
    }
  }


}
