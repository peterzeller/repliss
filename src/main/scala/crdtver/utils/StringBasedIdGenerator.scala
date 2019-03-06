package crdtver.utils

import scala.collection.mutable

class StringBasedIdGenerator[S, T](generate: Int => T) extends (S => T) with mutable.Iterable[(S,T)] {
  private var id = 0
  private val forward = new mutable.LinkedHashMap[String, T]()
  private val reverse = new mutable.LinkedHashMap[T, S]()
  private var frozen = false

  override def apply(v: S): T = {
    val vString = v.toString
    forward.getOrElseUpdate(vString, {
      if (frozen)
        throw new RuntimeException(s"Generating new id for $v after it is frozen")
      id += 1
      val t = generate(id)
      forward.put(vString, t)
      reverse.put(t, v)
      t
    })
  }

  def keys: Iterable[S] = reverse.values

  def freeze(): Unit = {
    frozen = true
  }

  override def iterator: Iterator[(S, T)] = reverse.iterator.map(x => (x._2, x._1))
}
