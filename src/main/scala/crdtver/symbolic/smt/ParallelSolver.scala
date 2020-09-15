package crdtver.symbolic.smt

import java.time.{Duration, LocalDateTime, Period}
import java.time.temporal.TemporalAmount

import codes.reactive.scalatime._
import crdtver.symbolic.smt.Smt.NamedConstraint
import crdtver.symbolic.smt.Solver._
import crdtver.utils.{ConcurrencyUtils, TimeTaker}
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.ListExtensions.ListUtils

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered


/**
 * Runs the given solvers concurrently, returning the first (fastest) non-unknown result
 */
class ParallelSolver(
  subSolvers: List[Solver]
) extends Solver {
  require(subSolvers.nonEmpty)


  def debugPrint(s: => String): Unit = {
//    println(s)
  }

  override def toString: String = s"(${subSolvers.mkString("|")})"

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    runConcurrent(constraints, options, name)
  }

  var run = 0

  def runConcurrent(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    run += 1
    val runL = run
    import scala.concurrent.ExecutionContext.Implicits.global
    val results: List[ConcurrencyUtils.Task[CheckRes]] =
      for ((subSolver, i) <- subSolvers.zipWithIndex) yield {
        val name2 = s"${name}_solver$i"
        ConcurrencyUtils.spawn(
          name = name2,
          work = () => {
            debugPrint(s"$runL) starting $subSolver")
            val res = subSolver.check(constraints, options, name)
//            if (!Thread.currentThread().isInterrupted)
            debugPrint(s"$runL) finished $subSolver --> $res")
            res
          })
      }
    try {
      val (dur, firstResult: Option[(CheckRes, Int)]) = TimeTaker.measure { () =>
        ConcurrencyUtils.raceWithIndex(results).find(!_._1.isUnknown) }
      firstResult match {
        case Some((r, i)) =>
          val s = subSolvers(i)
          debugPrint(s"$run) [${dur.formatH}] solved ${constraints.size} using $s --> $r")
        case None =>
      }
      firstResult.map(_._1).getOrElse(Unknown())
    } finally {
      // cancel remaining executions
      results.foreach(_.cancel())
    }
  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String = subSolvers.head.exportConstraints(assertions, options)
}
