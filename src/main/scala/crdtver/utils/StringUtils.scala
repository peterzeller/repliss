package crdtver.utils

import java.io.InputStream


object StringUtils {

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
