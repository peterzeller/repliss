package crdtver.utils

import scala.collection.mutable

class myMemo[K, V](f: K => V) extends (K => V) with Iterable[(K, V)] {
  private val cache = new mutable.LinkedHashMap[K, V]()

  override def apply(key: K): V = {
    cache.getOrElseUpdate(key, f(key))
  }

  def keySet(): collection.Set[K] = cache.keySet

  def values(): Iterable[V] = cache.values

  override def iterator: Iterator[(K, V)] = cache.iterator
}