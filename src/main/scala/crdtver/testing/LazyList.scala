package crdtver.testing


import crdtver.testing
import crdtver.testing.LazyList.Empty

import scala.collection.generic.{CanBuildFrom, GenericTraversableTemplate, TraversableFactory}
import scala.collection.immutable.Stream
import scala.collection.{GenIterable, GenTraversableOnce, IterableLike, TraversableOnce, mutable}
import scala.language.implicitConversions

final class LazyList[+A] private(private[this] var lazyState: () => LazyList.State[A]) {



  def force: Unit = iterator.foreach(x => {})

  import LazyList._


  def length: Int = iterator.length



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

  def isEmpty: Boolean = state match {
    case State.Empty => true
    case _: State.Cons[_] => false
  }

  def nonEmpty: Boolean = !isEmpty

  def head: A = this match {
    case x #:: xs => x
  }

  def tail: LazyList[A] = this match {
    case x #:: xs => xs
  }



  def flatMap[B](f: A => LazyList[B]): LazyList[B] = newLL {
    if (isEmpty) {
      State.Empty
    } else {
      (f(state.head) #::: state.tail.flatMap(f)).state
    }
  }

  def zipWithIndex: LazyList[(A, Int)] = {
    def impl(l: LazyList[A], n: Int): LazyList[(A, Int)] =
    newLL {
      if (l.isEmpty) {
        State.Empty
      } else {
        new State.Cons((l.state.head, n), impl(l.state.tail, n+1))
      }
    }
    impl(this, 0)
  }


  def zip[B](other: LazyList[B]): LazyList[(A, B)]=
    newLL{
        if (isEmpty || other.isEmpty)
          State.Empty
        else
          new LazyList.State.Cons((head, other.head), tail.zip(other.tail))
      }




//  def flatMap[B](f: A => TraversableOnce[B]): LazyList[B] = newLL {
//    this match {
//      case Empty() =>
//        State.Empty
//      case x #:: xs =>
//        (fromTraversable(f(x)) #::: xs.flatMap(f)).state
//    }
//  }

  def map[B](f: A => B): LazyList[B] = newLL {
    this match {
      case Empty() =>
        State.Empty
      case x #:: xs =>
        (f(x) #:: xs.map(f)).state
    }
  }

  @scala.annotation.tailrec
  def foreach[U](f: A => U): Unit = this match {
    case Empty() =>
    case x #:: xs =>
      f(x)
      xs.foreach(f)
  }


  @scala.annotation.tailrec
  def forall(p: A => Boolean): Boolean = this match {
    case Empty() => true
    case x #:: xs =>
      p(x) && xs.forall(p)
  }

  @scala.annotation.tailrec
  def exists(p: A => Boolean): Boolean = this match {
    case Empty() => false
    case x #:: xs =>
      p(x) || xs.exists(p)
  }


  @scala.annotation.tailrec
  def find(p: A => Boolean): Option[A] = this match {
    case Empty() => None
    case x #:: xs =>
      if (p(x)) Some(x)
      else xs.find(p)
  }

  def filterNot(p: A => Boolean): LazyList[A] = filter(x => !p(x))

  def filter(p: A => Boolean): LazyList[A] =
    newLL {
      def filterState(state: LazyList.State[A]): State[A] = state match {
        case State.Empty =>
          State.Empty
        case _ : State.Cons[_] =>
          val x = state.head
          val xs = state.tail
          if (p(x))
            filterState(xs.state)
          else
            new LazyList.State.Cons(x, xs)
      }
      filterState(state)
    }

  def withFilter(p: A => Boolean): LazyList[A] = filter(p)



  def take(n: Int): LazyList[A] =
    if (n <= 0) LazyList.empty
    else newLL {
      this match {
        case Empty() => State.Empty
        case x #:: xs =>
          new State.Cons(x, xs.take(n - 1))
      }
    }

  def drop(n: Int): LazyList[A] =
    if (n <= 0) this
    else newLL {
      this match {
        case Empty() => State.Empty
        case x #:: xs =>
          xs.drop(n - 1).state
      }
    }

  def takeWhile(p: A => Boolean): LazyList[A] =
    newLL {
      this match {
        case Empty() => State.Empty
        case x #:: xs =>
          if (p(x))
            new State.Cons(x, xs.takeWhile(p))
          else
            State.Empty
      }
    }

  /** eager split */
  def splitAt(n: Int): (LazyList[A], LazyList[A]) =
    if (n <= 0) (LazyList.empty, this)
    else {
      this match {
        case Empty() => (empty, empty)
        case x #:: xs =>
          val (as, bs) = xs.splitAt(n - 1)
          (x#::as, bs)
      }
    }

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {

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
        b.append("<not computed>")
      }
    }

    b.append(start)
    traverse(this, first = true)
    b.append(end)
    b
  }

  override def toString: String = {
    val b = new StringBuilder
    addString(b, "LazyList(", ", ", ")")
    b.toString()
  }

  def mkString: String = mkString(", ")

  def mkString(sep: String): String = {
    val b = new StringBuilder
    addString(b, "", sep, "")
    b.toString()
  }

  def toList: List[A] =
    iterator.toList

  def iterator: Iterator[A] = {

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

object LazyList {

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

  implicit def toDeferrer[A](l: => LazyList[A]): Deferrer[A] = new Deferrer[A](() => l)

  final class Deferrer[A] private[LazyList] (private val l: () => LazyList[A]) extends AnyVal {

    def #::[B >: A](head: => B): LazyList[B] = {
      println(s"calling #::")
      newLL {
         new State.Cons[B](head, l())
      }
    }

    def #:::[B >: A](heads: LazyList[B]): LazyList[B] = newLL(consStates(heads, l()))

    def ++[B >: A](heads: LazyList[B]): LazyList[B] = heads #::: l()

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

  def fromTraversable[A](xs: TraversableOnce[A]): LazyList[A] = fromIterator(xs.toIterator)



  def apply[A](elems: A*): LazyList[A] = {
    var res: LazyList[A] = empty
    for (e <- elems.reverseIterator) {
      val rest = res
      res = e #:: rest
    }
    res
  }


  def canBuildFromLazy[A]: CanBuildFrom[LazyList[A], A, LazyList[A]] =
    new CanBuildFrom[LazyList[A], A, LazyList[A]] {


      override def apply(from: LazyList[A]): mutable.Builder[A, LazyList[A]] = {
        new Builder()
      }

      override def apply(): mutable.Builder[A, LazyList[A]] =
        new Builder()

    }


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

  def newBuilder[A]: mutable.Builder[A, LazyList[A]] = new Builder



  def unfold[A](start: A)(f: A => Option[A]): LazyList[A] = {
    def iterateLazy(start: A): LazyList[A] = {
      newLL {
        f(start) match {
          case Some(n) =>
            new State.Cons[A](n, iterateLazy(n))
          case None =>
            State.Empty
        }
      }
    }
    new LazyList(
      () => new State.Cons[A](start, iterateLazy(start))
    )
  }

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

  def continually[A](e: A): LazyList[A] = {
    lazy val r: LazyList[A] =
      newLL {
        new State.Cons(e, r)
      }
    r
  }
  def from(start: Int): LazyList[Int] = iterate(start)(_+1)

  def from[A](it: Iterator[A]): LazyList[A] = fromIterator(it)

  def from[A](it: TraversableOnce[A]): LazyList[A] = fromTraversable(it)

  implicit class FlattenExt[A](ll: LazyList[LazyList[A]]) {
    def flatten: LazyList[A] =
      ll.flatMap[A]((x: LazyList[A]) => x)
  }

  def consStates[A](first: => LazyList[A], second: => LazyList[A]): LazyList.State[A] = {
    first.state match {
      case State.Empty =>
        second.state
      case value: State.Cons[A] =>
        new State.Cons[A](value.head, value.tail #::: second)
    }
  }

  implicit class EqExt[A](l: LazyList[A]) {
    def sameElements(other: LazyList[A]): Boolean =
      if (l.isEmpty)
        other.isEmpty
      else
        other.nonEmpty && (l.head == other.head) && l.tail.sameElements(other.tail)
  }


}