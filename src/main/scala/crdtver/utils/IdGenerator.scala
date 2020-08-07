package crdtver.utils

import scala.collection.mutable
import scala.util.matching.Regex

class IdGenerator[S, N, T](normalize: S => N, generate: Int => T) extends (S => T) with mutable.Iterable[(S, T)] {
  private var maxId = 100
  private var usedIndexes = Set[Int]()
  private val forward = new mutable.LinkedHashMap[N, T]()
  private val reverse = new mutable.LinkedHashMap[T, S]()
  private var frozen = false

  val regex: Regex = ".*([0-9]+)[^0-9]*".r

  override def apply(v: S): T = {
    val normalizedV = normalize(v)
    forward.getOrElseUpdate(normalizedV, {
      if (frozen)
        throw new RuntimeException(s"Generating new id for $v after it is frozen\nAvailable names: ${forward.keys.mkString(", ")}")

      val i: Int = normalizedV.toString match {
        // if the string contains a number at the end, try to use this number
        case regex(n) if !usedIndexes.contains(n.toInt) => n.toInt
        // otherwise create a new Id
        case _ => maxId + 1
      }
      maxId = Math.max(maxId, i)
      usedIndexes += i
      val t = generate(i)
      forward.put(normalizedV, t)
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
