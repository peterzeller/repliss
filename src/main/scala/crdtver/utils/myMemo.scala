package crdtver.utils

import scala.collection.mutable

class myMemo[K, V](f: K => V) extends (K => V) with Iterable[(K, V)] {
  private val cache = new mutable.LinkedHashMap[K, V]()
  private val currentlyCalculating = new mutable.LinkedHashSet[K]()

  override def apply(key: K): V = {
    cache.getOrElseUpdate(key, {
      if (currentlyCalculating.add(key)) {
        val res = f(key)
        currentlyCalculating.remove(key)
        res
      } else {
        throw new RuntimeException(s"Cyclic dependency when calculating value for $key\nCurrently active:\n - ${currentlyCalculating.mkString("\n - ")}")
      }
    })
  }

  def keySet(): collection.Set[K] = cache.keySet

  def values(): Iterable[V] = cache.values

  override def iterator: Iterator[(K, V)] = cache.iterator
}