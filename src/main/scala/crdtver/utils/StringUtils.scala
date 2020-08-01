package crdtver.utils

import java.io.InputStream


object StringUtils {
  def uniqueName(str: String, avoid: Set[String], i: Int = 0): String = {
    if (i == 0 && !avoid.contains(str))
      str
    else {
      val n = s"${str}_$i"
      if (avoid.contains(n))
        uniqueName(str, avoid, i + 1)
      else
        n
    }
  }


  implicit class StringExtensions(base: String) {
    def insertBeforeDot(insert: String): String = {
      val pos = base.lastIndexOf(".")
      if (pos >= 0)
        base.substring(0, pos) + insert + base.substring(pos)
      else
        base + insert

    }
  }

}
