package crdtver.utils

import java.time.Duration


object DurationUtils {

  implicit class DurationExt(d: Duration) {
    def formatH: String =
      d.toString
        .substring(2)
        .replaceAll("(\\d[HMS])(?!$)", "$1 ")
        .toLowerCase

    def toJava: Duration = ???
  }


}
