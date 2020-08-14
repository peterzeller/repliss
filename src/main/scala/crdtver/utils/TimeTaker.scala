package crdtver.utils

import java.time.{Duration, LocalDateTime}
import java.util.concurrent.ConcurrentHashMap

import crdtver.utils.DurationUtils.DurationExt

class TimeTaker {

  private var times: Map[String, Duration] = Map()

  def getTimes: Map[String, Duration] = times

  def measure[T](name: String)(body: () => T): T = {
    val (dur, res) = TimeTaker.measure(body)
    synchronized(this, {
      val newDur = times.getOrElse(name, Duration.ZERO).plus(dur)
      times += (name -> newDur)
    })
    res
  }

}

object TimeTaker {
  def measure[T](body: () => T): (Duration, T) = {
    val startTime = LocalDateTime.now()
    val res = body()
    val duration = Duration.between(startTime, LocalDateTime.now())
    (duration, res)
  }
}
