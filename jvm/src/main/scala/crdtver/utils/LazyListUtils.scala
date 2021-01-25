package crdtver.utils

import crdtver.testing.Interpreter


object LazyListUtils {

  trait Lazy[T] {
    def get: T
  }

  def Lazy[T](x: => T): Lazy[T] =
    new Lazy[T] {
      lazy val value: T = x

      override def get: T = value
    }


  /**
   * generates all lists that can be combined from the given head and tails
   */
  def combine[T](headF: => T, tailsF: => LazyList[LazyList[T]]): LazyList[LazyList[T]] = {
    // make sure every argument is only evaluated once
    lazy val head = headF
    lazy val tails = tailsF
    (head #:: (tails match {
      case LazyList() => LazyList()
      case t #:: ts => t
    })) #:: (tails match {
      case LazyList() => LazyList()
      case t #:: ts => combine(head, ts)
    })
  }


  /**
   * all combinations for elements in a list
   *
   * example:
   * list = [10,20,30]
   * f(x) = [x + 1, x + 2, x + 3]
   *
   * result: [11, 21, 31], [11, 21, 32], [11, 21, 33], [11, 22, 31], ...
   *
   */
  def listCases[A, B](list: List[A], f: A => LazyList[B]): LazyList[List[B]] = list match {
    case Nil =>
      LazyList(List())
    case x :: xs =>
      for {
        b <- f(x).breadthFirst
        rest <- listCases(xs, f)
      } yield b :: rest
  }


  /**
   * makes the flatmap breadth first
   **/
  class BreadthFirst[A](LazyList: LazyList[A]) {
    def flatMap[B](f: A => LazyList[B]): LazyList[B] = {
      val LazyLists: LazyList[LazyList[B]] = LazyList.map(f)
      LazyLists.flattenBreadthFirst
    }
  }


  implicit class LazyListOfLazyListExtensions[A](ll: LazyList[LazyList[A]]) {
    def flattenDepthFirst: LazyList[A] = ll.flatten

    def flattenBreadthFirst: LazyList[A] = {
      val nonempty = ll.filter(_.nonEmpty)
      if (nonempty.isEmpty)
        LazyList()
      else
        nonempty.map(_.head) #::: nonempty.map(_.tail).flattenBreadthFirst
    }

    def flattenDiagonal(breadth: Int): LazyList[A] = {
      def walk(s: LazyList[LazyList[A]], n: Int): LazyList[A] = {
        val nonempty = ll.filter(_.nonEmpty)
        val (start, rest) = nonempty.splitAt(breadth)
        val heads = start.map(_.head)
        val tails = start.map(_.tail)
        heads ++ (tails ++ rest).flattenDiagonal(n + breadth)
      }

      walk(ll, breadth)
    }


  }

  def allCombinations[A](lists: List[LazyList[A]]): LazyList[List[A]] = {
    lists match {
      case Nil => LazyList(List())
      case x :: xs =>
        for {
          rest <- allCombinations(xs)
          xc <- x
        } yield xc :: rest
    }
  }


  implicit class LazyListExtensions[A](ll: LazyList[A]) {

    def breadthFirst: BreadthFirst[A] = new BreadthFirst(ll)

    /** [1,2,3] cartesian [a,b] == [(1,a), (1,b), (2, a), (2,b), (3,a), (3,b)] */
    def cartesian[B](other: LazyList[B]): LazyList[(A, B)] =
      for {
        a <- ll
        b <- other
      } yield (a, b)

    /** [1,2,3] cartesianR [a,b] == [(1,a), (2, a), (3,a), , (1,b), (2,b), (3,b)] */
    def cartesianR[B](other: LazyList[B]): LazyList[(A, B)] =
      for {
        b <- other
        a <- ll
      } yield (a, b)


    /** bundles elements together */
    def bundled(size: Int): LazyList[LazyList[A]] = {
      val (first, second) = ll.splitAt(size)
      if (first.isEmpty) LazyList()
      else first #:: second.bundled(size)
    }

    /** takes all elements until a condition is true (also includes the first element where the condition is true) */
    def takeUntil(p: A => Boolean): LazyList[A] = {
      LazyList.unfold((ll, None: Option[A])) { s =>
        s._2 match {
          case Some(prev) if p(prev) =>
            None
          case _ =>
            if (s._1.isEmpty) {
              None
            } else {
              val head = s._1.head
              val tail = s._1.tail
              Some(head, (tail, Some(head)))
            }
        }
      }
    }


  }

  def listsWithSize[T](size: Int, elems: LazyList[T]): LazyList[List[T]] = {
    require(size >= 0)
    if (size == 0)
      LazyList(List())
    else
      for {
        hd <- elems
        tl <- listsWithSize(size - 1, elems)
      } yield hd :: tl
  }

  def listsUpToSize[T](maxSize: Int, elems: LazyList[T]): LazyList[List[T]] = {
    (0 to maxSize).to(LazyList).flatMap(size => listsWithSize(size, elems))
  }

  def allSubsets[T](set: Iterable[T]): LazyList[Set[T]] = {
    val list = set.toList
    for (is <- allCombinations(list.map(_ => LazyList(false, true)))) yield
      list.zip(is).filter(_._2).map(_._1).toSet
  }

}
