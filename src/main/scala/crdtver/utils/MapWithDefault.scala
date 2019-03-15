package crdtver.utils

import scala.collection.immutable.Map.WithDefault

class MapWithDefault[K,V](map: Map[K,V], default: V) extends Map[K,V] {
  override def +[V1 >: V](kv: (K, V1)): Map[K, V1] =
    new MapWithDefault(map + kv, default)

  override def get(key: K): Option[V] =
    Some(map.getOrElse(key, default))

  override def iterator: Iterator[(K, V)] =
    map.iterator

  override def -(key: K): Map[K, V] =
    new MapWithDefault(map - key, default)

  override def toString(): String =
    s"""MapWithDefault(${map.map(e => e._1 + " -> " + e._2).mkString(", ")}, default = $default)"""
}
