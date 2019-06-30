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

}
