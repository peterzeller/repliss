package repliss.js

import scala.collection.mutable.ListBuffer

object Utils {

  def unescape(input: String): String = {
      val replacements = List(
        "&lt;" -> "<",
        "&gt;" -> ">",
        "&quot;" -> "\"",
        "&amp;" -> "&"
      )
      var res = input
      for ((x,y) <- replacements) {
        res = res.replace(x, y)
      }
      res
    }

  def uniqueKeys[T](l: List[T])(getter: T => String)(setter: (T, String) => T): List[T] = {
    var keys = Set[String]()
    for (t <- l) yield {
      var key = getter(t)
      var i = 0
      while (keys contains key) {
        i += 1
        key = getter(t) + "_" + i
      }
      keys += key
      setter(t, key)
    }
  }

}
