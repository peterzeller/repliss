package crdtver.utils

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.List
import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

object ListExtensions {

  implicit class ListUtils[T](list: List[T]) {

    def makeMap[V](f: T => V): Map[T, V] =
      list.map(k => k -> f(k)).toMap

    /** groupBy with mapping of values */
    def groupBy2[K, V](key: T => K, value: T => V): Map[K,List[V]] =
      list.groupBy(key).view.mapValues(l => l.map(value)).toMap




    def pairs: List[(T,T)] = list match {
      case List() => List()
      case List(x) => List()
      case x::y::xs => (x,y) :: (y::xs).pairs
    }


    def doForFirst[K](f: PartialFunction[T, K]): Option[K] = {
      for (x <- list) {
        f.unapply(x) match {
          case r@Some(_) =>
            return r
          case None =>
        }
      }
      None
    }
  }

  implicit class KVListUtils[K,V](list: List[(K,V)]) {

    def toMap2: Map[K, List[V]] =
      list.groupBy2(_._1, _._2)

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
