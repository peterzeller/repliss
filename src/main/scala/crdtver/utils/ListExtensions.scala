package crdtver.utils

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.List
import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

object ListExtensions {

  implicit class ListUtils[T](list: List[T]) {

    def makeMap[V](f: T => V): Map[T, V] =
      list.map(k => k -> f(k))

  }

  implicit def buildMap[A, B, C]: CanBuildFrom[List[A], (B, C), Map[B, C]] =
    new CanBuildFrom[List[A], (B, C), Map[B, C]] {
      override def apply(from: List[A]): mutable.Builder[(B, C), Map[B, C]] =
        immutable.Map.newBuilder

      override def apply(): mutable.Builder[(B, C), Map[B, C]] =
        immutable.Map.newBuilder
    }

  implicit def buildArray[A, B: ClassTag]: CanBuildFrom[List[A], B, Array[B]] =
    new CanBuildFrom[List[A], B, Array[B]] {


      override def apply(from: List[A]): mutable.ArrayBuilder[B] = {
        val res = mutable.ArrayBuilder.make[B]()
        res.sizeHint(from)
        res
      }

      override def apply(): mutable.Builder[B, Array[B]] = mutable.ArrayBuilder.make()
    }

  /**
    * All combinations of splitting a list L into L == xs ++ List(a) ++ List(ys)
    */
  def splitOffOne[T](list: List[T]): List[(List[T], T, List[T])] = list match {
    case List() => List()
    case x::xs =>
      val rest: List[(List[T], T, List[T])] = splitOffOne(xs).map { case (f, a, b) => (x :: f, a, b) }
      (List(), x, xs) :: rest
  }


}