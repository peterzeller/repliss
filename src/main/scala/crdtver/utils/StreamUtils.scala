package crdtver.utils

object StreamUtils {



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
  def listCases[A,B](list: List[A], f: A => Stream[B]): Stream[List[B]] = list match {
    case Nil =>
      Stream(List())
    case x::xs =>
      for {
        b <- f(x).breadthFirst
        rest <- listCases(xs, f)
      } yield b::rest
  }


  /**
    * makes the flatmap breadth first
    **/
  class BreadthFirst[A](stream: Stream[A]) {
    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      val streams: Stream[Stream[B]] = stream.map(f)
      streams.flattenBreadthFirst
    }
  }


  implicit class StreamOfStreamExtensions[A](stream: Stream[Stream[A]]) {
    def flattenDepthFirst: Stream[A] = stream.flatten

    def flattenBreadthFirst: Stream[A] = {
      val nonempty = stream.filter(_.nonEmpty)
      val heads = nonempty.map(_.head)
      val tails = nonempty.map(_.tail)
      heads ++ tails.flattenBreadthFirst
    }

    def flattenDiagonal(breadth: Int): Stream[A] = {
      def walk(s: Stream[Stream[A]], n: Int): Stream[A] = {
        val nonempty = stream.filter(_.nonEmpty)
        val (start, rest) = nonempty.splitAt(breadth)
        val heads = start.map(_.head)
        val tails = start.map(_.tail)
        heads ++ (tails ++ rest).flattenDiagonal(n + breadth)
      }
      walk(stream, breadth)
    }



  }

  implicit class StreamExtensions[A](stream: Stream[A]) {

    def breadthFirst: BreadthFirst[A] = new BreadthFirst(stream)

    /** [1,2,3] cartesian [a,b] == [(1,a), (1,b), (2, a), (2,b), (3,a), (3,b)] */
    def cartesian[B](other: Stream[B]): Stream[(A,B)] =
      for {
        a <- stream
        b <- other
      } yield (a, b)

    /** [1,2,3] cartesianR [a,b] == [(1,a), (2, a), (3,a), , (1,b), (2,b), (3,b)] */
    def cartesianR[B](other: Stream[B]): Stream[(A,B)] =
      for {
        b <- other
        a <- stream
      } yield (a, b)


  }

}
