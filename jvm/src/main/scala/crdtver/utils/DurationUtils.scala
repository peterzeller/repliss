package crdtver.utils

import java.time.Duration
import java.time.temporal.{ChronoUnit, Temporal, TemporalAmount}

import scopt.Read


object DurationUtils {
  def maxDuration: Duration = Duration.ofSeconds(Long.MaxValue, 0)

  def read: Read[Duration] = (implicitly[Read[scala.concurrent.duration.Duration]]).map(_.toJava)


  implicit class DurationExt(d: Duration) {
    def formatH: String =
      d.toString
        .substring(2)
        .replaceAll("(\\d[HMS])(?!$)", "$1 ")
        .toLowerCase

    def *(f: Double): Duration = {
      val m = d.toMillis
      Duration.of((m * f).toLong, ChronoUnit.MILLIS)
    }

    def /(o: Duration): Double =
      d.toMillis.toDouble / o.toMillis

  }

  implicit class ScalaDurationExt(d: scala.concurrent.duration.Duration) {
    def toJava: Duration =
      Duration.of(d.toMillis, ChronoUnit.MILLIS)
  }


  implicit class DurationUnits(d: Int) {
    def minutes: Duration = Duration.of(d, ChronoUnit.MINUTES)

    def seconds: Duration = Duration.of(d, ChronoUnit.SECONDS)

    def ms: Duration = Duration.of(d, ChronoUnit.MILLIS)
  }

  def Inf: Duration = Duration.ofSeconds(Long.MaxValue)

}
