package crdtver.utils

import crdtver.testing.LazyList

object LazyListUtils {



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
  def listCases[A,B](list: List[A], f: A => LazyList[B]): LazyList[List[B]] = list match {
    case Nil =>
      LazyList(List())
    case x::xs =>
      for {
        b <- f(x).breadthFirst
        rest <- listCases(xs, f)
      } yield b::rest
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
      val heads = nonempty.map(_.head)
      val tails = nonempty.map(_.tail)
      heads ++ tails.flattenBreadthFirst
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

  implicit class LazyListExtensions[A](ll: LazyList[A]) {

    def breadthFirst: BreadthFirst[A] = new BreadthFirst(ll)

    /** [1,2,3] cartesian [a,b] == [(1,a), (1,b), (2, a), (2,b), (3,a), (3,b)] */
    def cartesian[B](other: LazyList[B]): LazyList[(A,B)] =
      for {
        a <- ll
        b <- other
      } yield (a, b)

    /** [1,2,3] cartesianR [a,b] == [(1,a), (2, a), (3,a), , (1,b), (2,b), (3,b)] */
    def cartesianR[B](other: LazyList[B]): LazyList[(A,B)] =
      for {
        b <- other
        a <- ll
      } yield (a, b)


  }

}
