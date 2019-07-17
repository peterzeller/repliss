package crdtver.testing


import crdtver.testing

import scala.collection.generic.{CanBuildFrom, GenericTraversableTemplate, TraversableFactory}
import scala.collection.immutable.Stream
import scala.collection.{GenIterable, GenTraversableOnce, IterableLike, TraversableOnce, mutable}
import scala.language.implicitConversions

final class LazyList[+A] private(private[this] var lazyState: () => LazyList.State[A])
  extends Iterable[A]
    with TraversableOnce[A]
    with GenIterable[A]
    with IterableLike[A, LazyList[A]]
    with GenericTraversableTemplate[A, LazyList] {

  override def companion: LazyList.type = LazyList

  import LazyList._

  @volatile private var stateEvaluated: Boolean = false

  @inline private def stateDefined: Boolean = stateEvaluated

  private lazy val state: LazyList.State[A] = {
    val res = lazyState()
    // if we set it to `true` before evaluating, we may infinite loop
    // if something expects `state` to already be evaluated
    stateEvaluated = true
    lazyState = null // allow GC
    res
  }

  override def isEmpty: Boolean = state match {
    case State.Empty => true
    case _: State.Cons[_] => false
  }


//  def flatMap[B](f: A => TraversableOnce[B]): LazyList[B] = newLL {
//    this match {
//      case Empty() =>
//        State.Empty
//      case x #:: xs =>
//        (fromTraversable(f(x)) #::: xs.flatMap(f)).state
//    }
//  }

//  def map[B](f: A => B): LazyList[B] = newLL {
//    this match {
//      case Empty() =>
//        State.Empty
//      case x #:: xs =>
//        (f(x) #:: xs.map(f)).state
//    }
//  }

//  @scala.annotation.tailrec
//  override def foreach[U](f: A => U): Unit = this match {
//    case Empty() =>
//    case x #:: xs =>
//      f(x)
//      xs.foreach(f)
//  }

  override def hasDefiniteSize: Boolean = false

  @scala.annotation.tailrec
  override def forall(p: A => Boolean): Boolean = this match {
    case Empty() => true
    case x #:: xs =>
      p(x) && xs.forall(p)
  }

  @scala.annotation.tailrec
  override def exists(p: A => Boolean): Boolean = this match {
    case Empty() => false
    case x #:: xs =>
      p(x) || xs.exists(p)
  }


  @scala.annotation.tailrec
  override def find(p: A => Boolean): Option[A] = this match {
    case Empty() => None
    case x #:: xs =>
      if (p(x)) Some(x)
      else xs.find(p)
  }

  override def take(n: Int): LazyList[A] =
    if (n <= 0) LazyList.empty
    else newLL {
      this match {
        case Empty() => State.Empty
        case x #:: xs =>
          new State.Cons(x, xs.take(n - 1))
      }
    }

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {

    def traverse(e: LazyList[A], first: Boolean): Unit = {
      if (!first)
        b.append(sep)
      if (e.stateEvaluated) {
        e match {
          case Empty() =>
          case x #:: xs =>
            b.append(x)
            traverse(xs, first = false)
        }
      } else {
        b.append("...")
      }
    }

    b.append(start)
    traverse(this, first = true)
    b.append(end)
    b
  }

  override def iterator: Iterator[A] = {

    var current = this
    new Iterator[A] {
      override def hasNext: Boolean = !current.isEmpty


      override def next(): A = {
        current match {
          case Empty() => throw new IllegalStateException("no next element in LazyList")
          case x #:: xs =>
            current = xs
            x
        }
      }
    }
  }
}

object LazyList extends TraversableFactory[LazyList] {

  val empty = new LazyList(() => State.Empty)


  sealed trait State[+A] {
    def head: A

    def tail: LazyList[A]
  }

  private def newLL[A](state: => State[A]): LazyList[A] = new LazyList(() => state)

  private object State {

    object Empty extends State[Nothing] {
      def head: Nothing = throw new NoSuchElementException("head of empty lazy list")

      def tail: LazyList[Nothing] = throw new UnsupportedOperationException("tail of empty lazy list")
    }

    final class Cons[A](val head: A, val tail: LazyList[A]) extends State[A]

  }

  implicit class LazyExtensions[A](l: => LazyList[A]) {
    def #::(head: => A): LazyList[A] = newLL {
      new State.Cons[A](head, l)
    }

    def #:::(heads: LazyList[A]): LazyList[A] = newLL {
      heads.state match {
        case State.Empty =>
          l.state
        case value: State.Cons[A] =>
          new State.Cons[A](value.head, value.tail #::: l)
      }
    }
  }

  object Empty {
    def unapply[A](l: LazyList[A]): Boolean = l.isEmpty
  }

  object #:: {
    def unapply[A](l: LazyList[A]): Option[(A, LazyList[A])] = l.state match {
      case State.Empty => None
      case value: State.Cons[A] => Some((value.head, value.tail))
    }
  }

  def fromIterator[A](it: Iterator[A]): LazyList[A] = newLL {
    if (it.hasNext) {
      new State.Cons(it.next(), fromIterator(it))
    } else {
      State.Empty
    }
  }

  def fromTraversable[A](xs: TraversableOnce[A]): LazyList[A] = xs match {
    case ll: LazyList[A] => ll
    case _ => fromIterator(xs.toIterator)
  }


  def singleton[A](elem: A): LazyList[A] = elem #::empty

  override def apply[A](elems: A*): LazyList[A] = {
    fromTraversable(elems)
  }

  def canBuildFromLazy[A]: CanBuildFrom[LazyList[A], A, LazyList[A]] =
    new CanBuildFrom[LazyList[A], A, LazyList[A]] {


      override def apply(from: LazyList[A]): mutable.Builder[A, LazyList[A]] = {
        new Builder()
      }

      override def apply(): mutable.Builder[A, LazyList[A]] =
        new Builder()

    }

  class LazyListCanBuildFrom[A] extends GenericCanBuildFrom[A]

  def canBuildFromColl[A]: CanBuildFrom[Coll, A, LazyList[A]] = new LazyListCanBuildFrom[A]

  implicit def canBuildFromTraversableOnce[X, A]: CanBuildFrom[TraversableOnce[X], A, LazyList[A]] =
    new CanBuildFrom[TraversableOnce[X], A, LazyList[A]] {

      override def apply(from: TraversableOnce[X]): mutable.Builder[A, LazyList[A]] = {
        val res = new Builder[A]
        res
      }

      override def apply(): mutable.Builder[A, LazyList[A]] =
        new Builder

    }

  class Builder[A](var res: LazyList[A] = LazyList.empty)
    extends mutable.Builder[A, LazyList[A]] {


    override def ++=(xs: TraversableOnce[A]): Builder.this.type = {
      res = res #::: fromTraversable(xs)
      this
    }

    override def +=(elem: A): Builder.this.type = {
      res = res #::: LazyList(elem)
      this
    }

    override def clear(): Unit = res =
      LazyList.empty

    override def result(): LazyList[A] = {
      res
    }
  }

  override def newBuilder[A]: mutable.Builder[A, LazyList[A]] = new Builder

  def iterate[A](start: A)(f: A => A): LazyList[A] = {
    def iterateLazy(start: A): LazyList[A] = {
      newLL {
        val n = f(start)
        new State.Cons[A](n, iterateLazy(n))
      }
    }
    new LazyList(
      () => new State.Cons[A](start, iterateLazy(start))
    )
  }



}