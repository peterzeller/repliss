package crdtver.utils

import cats.effect.{Concurrent, IO}
import crdtver.Repliss

import scala.concurrent.Future
import scala.language.higherKinds


object StreamUtils {
  def fromLazyList[F[_], T](list: LazyList[T]): fs2.Stream[F, T] = list match {
    case LazyList() =>
      fs2.Stream.empty
    case x #:: xs =>
      fs2.Stream.emit(x) ++ fromLazyList(xs)
  }

 def fromLazyListIO[T](list: LazyList[T]): fs2.Stream[IO, T] =
   fs2.Stream.unfoldEval[IO, LazyList[T], T](list)((l: LazyList[T]) => IO {
     l match {
       case LazyList() =>
         None
       case x #:: xs =>
         Some((x, xs))
     }
   })


  def raceInterleave[F[_], T](a: fs2.Stream[F, T], b: fs2.Stream[F, T])(implicit c: Concurrent[F]): fs2.Stream[F, T] = {
    a.merge(b)
  }

//  def fromFuture[F[_], O](fut: Future[O]): fs2.Stream[F, O] = {
//    IO.fromFuture(fut)
//  }




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
  class BreadthFirst[A](stream: LazyList[A]) {
    def flatMap[B](f: A => LazyList[B]): LazyList[B] = {
      val streams: LazyList[LazyList[B]] = stream.map(f)
      streams.flattenBreadthFirst
    }
  }


  implicit class StreamOfStreamExtensions[A](stream: LazyList[LazyList[A]]) {
    def flattenDepthFirst: LazyList[A] = stream.flatten

    def flattenBreadthFirst: LazyList[A] = {
      val nonempty = stream.filter(_.nonEmpty)
      val heads = nonempty.map(_.head)
      val tails = nonempty.map(_.tail)
      heads ++ tails.flattenBreadthFirst
    }

    def flattenDiagonal(breadth: Int): LazyList[A] = {
      def walk(s: LazyList[LazyList[A]], n: Int): LazyList[A] = {
        val nonempty = stream.filter(_.nonEmpty)
        val (start, rest) = nonempty.splitAt(breadth)
        val heads = start.map(_.head)
        val tails = start.map(_.tail)
        heads ++ (tails ++ rest).flattenDiagonal(n + breadth)
      }
      walk(stream, breadth)
    }



  }

  implicit class StreamExtensions[A](stream: LazyList[A]) {

    def breadthFirst: BreadthFirst[A] = new BreadthFirst(stream)

    /** [1,2,3] cartesian [a,b] == [(1,a), (1,b), (2, a), (2,b), (3,a), (3,b)] */
    def cartesian[B](other: LazyList[B]): LazyList[(A,B)] =
      for {
        a <- stream
        b <- other
      } yield (a, b)

    /** [1,2,3] cartesianR [a,b] == [(1,a), (2, a), (3,a), , (1,b), (2,b), (3,b)] */
    def cartesianR[B](other: LazyList[B]): LazyList[(A,B)] =
      for {
        b <- other
        a <- stream
      } yield (a, b)


  }

}
