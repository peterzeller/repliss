package crdtver.testing


import java.util.concurrent.ArrayBlockingQueue

import crdtver.testing.LazyList2.{newLL, sCons, stateFromIterator}

import scala.collection.{AbstractSeq, GenTraversableOnce, LinearSeqOptimized, mutable}
import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import scala.collection.immutable.{LinearSeq, Stream}
import scala.language.implicitConversions

final class LazyList2[+A] private(private[this] var lazyState: () => LazyList2.State[A])
  extends AbstractSeq[A]
    with LinearSeq[A]
    with GenericTraversableTemplate[A, LazyList2]
    with LinearSeqOptimized[A, LazyList2[A]]{

  import crdtver.testing.LazyList2.State

  override def companion: GenericCompanion[LazyList2] = LazyList2

  @volatile private[this] var stateEvaluated: Boolean = false
  @inline private def stateDefined: Boolean = stateEvaluated

  private lazy val state: State[A] = {
      val res = lazyState()
      // if we set it to `true` before evaluating, we may infinite loop
      // if something expects `state` to already be evaluated
      stateEvaluated = true
      lazyState = null // allow GC
      res
    }

  override def isEmpty: Boolean = state eq State.Empty

  override def head: A = state.head

  override def tail: LazyList2[A] = state.tail


  @inline private[this] def knownIsEmpty: Boolean = stateEvaluated && (isEmpty: @inline)
  @inline private def knownNonEmpty: Boolean = stateEvaluated && !(isEmpty: @inline)

  def knownSize: Int = if (knownIsEmpty) 0 else -1

  /** The lazy list resulting from the concatenation of this lazy list with the argument lazy list.
    *
    * $preservesLaziness
    *
    * $appendStackSafety
    *
    * @param suffix The collection that gets appended to this lazy list
    * @return The lazy list containing elements of this lazy list and the iterable object.
    */
  def lazyAppendedAll[B >: A](suffix: => collection.Iterable[B]): LazyList2[B] =
    newLL {
      if (isEmpty) suffix match {
        case lazyList: LazyList2[B]       => lazyList.state // don't recompute the LazyList2
        case _                           => stateFromIterator(suffix.iterator)
      }
      else sCons(head, tail lazyAppendedAll suffix)
    }

  override def foreach[U](f: A => U): Unit = {
    super.foreach(f)
  }

  override def take(n: Int): LazyList2[A] = super.take(n)


}

object LazyList2 extends SeqFactory[LazyList2] {

  sealed trait State[+A] {
    def head: A
    def tail: LazyList2[A]
  }

  private object State {
    object Empty extends State[Nothing] {
      def head: Nothing = throw new NoSuchElementException("head of empty lazy list")
      def tail: LazyList2[Nothing] = throw new UnsupportedOperationException("tail of empty lazy list")
    }

    final class Cons[A](val head: A, val tail: LazyList2[A]) extends State[A]
  }

  implicit def toDeferrer[A](l: => LazyList2[A]): Deferrer[A] = new Deferrer[A](() => l)

  final class Deferrer[A] private[LazyList2] (private val l: () => LazyList2[A]) extends AnyVal {
    /** Construct a LazyList2 consisting of a given first element followed by elements
      *  from another LazyList2.
      */
    def #:: [B >: A](elem: => B): LazyList2[B] = newLL(sCons(elem, l()))
    /** Construct a LazyList2 consisting of the concatenation of the given LazyList2 and
      *  another LazyList2.
      */
    def #:::[B >: A](prefix: LazyList2[B]): LazyList2[B] = prefix lazyAppendedAll l()
  }


  /** Creates a new LazyList2. */
  @inline private def newLL[A](state: => State[A]): LazyList2[A] = new LazyList2[A](() => state)

  /** Creates a new State.Cons. */
  @inline private def sCons[A](hd: A, tl: LazyList2[A]): State[A] = new State.Cons[A](hd, tl)

  /** Creates a State from an Iterator, with another State appended after the Iterator
    * is empty.
    */
  private def stateFromIteratorConcatSuffix[A](it: Iterator[A])(suffix: => State[A]): State[A] =
    if (it.hasNext) sCons(it.next(), newLL(stateFromIteratorConcatSuffix(it)(suffix)))
    else suffix

  /** Creates a State from an IterableOnce. */
  private def stateFromIterator[A](it: Iterator[A]): State[A] =
    if (it.hasNext) sCons(it.next(), newLL(stateFromIterator(it)))
    else State.Empty

  def fromIterable[A](iterable: TraversableOnce[A]): LazyList2[A] =
    newLL(stateFromIterator(iterable.toIterator))


  class LazyListCanBuildFrom[A] extends GenericCanBuildFrom[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LazyList2[A]] = new LazyListCanBuildFrom[A]

  override def apply[A](elems: A*): LazyList2[A] =
    elems.to(canBuildFrom)

  class StreamBuilder[A] extends mutable.Builder[A, LazyList2[A]] {
    var res = new java.util.concurrent.LinkedBlockingDeque[Option[A]]()



    override def +=(elem: A): this.type = {
      res.put(Some(elem))
      this
    }


    override def clear(): Unit = res.clear()

    override def result(): LazyList2[A] = {
      res.put(None)
      newLL{
        res.take() match {
          case Some(h) =>
            sCons(h, result())
          case None =>
            State.Empty
        }
      }
    }
  }

  override def newBuilder[A]: mutable.Builder[A, LazyList2[A]] = new StreamBuilder[A]()

  /** An infinite LazyList2 that repeatedly applies a given function to a start value.
    *
    *  @param start the start value of the LazyList2
    *  @param f     the function that's repeatedly applied
    *  @return      the LazyList2 returning the infinite sequence of values `start, f(start), f(f(start)), ...`
    */
  def iterate[A](start: => A)(f: A => A): LazyList2[A] =
    newLL {
      val head = start
      sCons(head, iterate(f(head))(f))
    }
}
