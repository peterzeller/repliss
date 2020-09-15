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
 * Runs a solver incrementally with increasing number of constraints.
 * Starting with most important constraints based on priority.
 *
 * increments define the steps.
 * starting with everything < increments(0),
 * then everything < increments(1)
 * and so on
 * and finally all
 */
class IncrementalSolver(
  subSolver: Solver,
  increments: Option[List[Int]],
) extends Solver {

  override def toString: String = s"I$subSolver"

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    val timeOut: Duration = options.extract { case SmtTimeout(t) => t }.getOrElse(2.minutes)
    val maxEndTime: LocalDateTime = LocalDateTime.now().plus(timeOut)
    val options2 = options.filter(!_.isInstanceOf[SmtTimeout])

    @tailrec
    def explore(activeConstraints: List[NamedConstraint], extraConstraints: List[NamedConstraint], lastModel: Option[Model]): CheckRes = {
      val timeoutDur = Duration.between(LocalDateTime.now(), maxEndTime)
      if (timeoutDur <= 0.seconds) {
        return Unknown()
      }
      val options3 = SmtTimeout(timeoutDur) :: options2

      // remove priority for subsolver:
      val activCeonstraintsNoPrio = activeConstraints.map(_.copy(priority = 0))
      subSolver.check(activCeonstraintsNoPrio, options3, name) match {
        case s: Satisfiable =>
          if (extraConstraints.isEmpty) {
            s
          } else {
            val model = s.getModel
            val minPrio = extraConstraints.head.priority

            val nextPrio = increments match {
              case Some(list) => list.find(_ >= minPrio).getOrElse(Int.MaxValue)
              case None => minPrio
            }
            println(s"Runing $subSolver with priority $nextPrio")

            // simple approach: just add all constraints with the next higher priority:
            val (newActive, newExtra) = extraConstraints.partition(_.priority <= nextPrio)
            explore(activeConstraints ++ newActive, newExtra, Some(model))

            // more complex approach (not working yet)

            // find the first constraint that is not satisfied by the model
//            val failedConstraint: Option[NamedConstraint] = extraConstraints.find { c =>
////              println(s"Trying ${c.description}")
//              model.evalQ(c.constraint).contains(false)
//            }

//            failedConstraint match {
//              case Some(constr) =>
//                println(s"Adding constraint ${constr.description}\n${constr.constraint}")
//                explore(constr :: activeConstraints, extraConstraints - constr, Some(model))
//
//              case None =>
//                println(s"All constraints satisfied")
//                // if all constraints are satisfied in the model, then
//                // s is already a complete model for all constraints
//                s
//            }
          }
        case Unknown() =>
          lastModel match {
            case Some(model) =>
              // use last model, although it might be incomplete
              new Satisfiable {
                override def isIncomplete: Boolean = true
                override def getModel: Model = model
              }
            case None =>
              Unknown()
          }
        case u =>
          u
      }


    }


    val firstPriority = increments.flatMap(_.headOption).getOrElse(0)
    val (activeConstraints, extraConstraints) = constraints.partition(_.priority == firstPriority)
    val extraConstraintsSorted = extraConstraints.sortBy(_.priority)
    explore(activeConstraints, extraConstraintsSorted, None)


  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String = subSolver.exportConstraints(assertions, options)
}
