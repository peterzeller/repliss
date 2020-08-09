package crdtver.symbolic.smt

import crdtver.symbolic.smt.Smt.NamedConstraint
import crdtver.symbolic.smt.Solver._
import crdtver.utils.ConcurrencyUtils
import crdtver.utils.ListExtensions.ListUtils

import scala.annotation.tailrec

case class CheckOptions(
  solver: Solver,
  extraOptions: List[SmtOption]
)

class IncrementalSolver(
  subSolvers: List[CheckOptions]
) extends Solver {
  require(subSolvers.nonEmpty)

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {


    @tailrec
    def explore(activeConstraints: List[NamedConstraint], extraConstraints: List[NamedConstraint], lastModel: Option[Model]): CheckRes = {
      runConcurrent(activeConstraints, options, name) match {
        case s: Satisfiable =>
          if (extraConstraints.isEmpty) {
            s
          } else {
            val model = s.getModel
            // simple approach: just add all constraints with the next higher priority:
            val (newActive, newExtra) = extraConstraints.partition(_.priority <= extraConstraints.head.priority)
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
              // TODO mark es spurious counter example since
              new Satisfiable {
                override def getModel: Model = model
              }
            case None =>
              Unknown()
          }
        case u =>
          u
      }


    }


    val (activeConstraints, extraConstraints) = constraints.partition(_.priority == 0)
    val extraConstraintsSorted = extraConstraints.sortBy(_.priority)
    explore(activeConstraints, extraConstraintsSorted, None)


  }

  def runConcurrent(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val results: List[ConcurrencyUtils.Task[CheckRes]] =
      for (subSolver <- subSolvers) yield {
        ConcurrencyUtils.spawn(
          name = name,
          work = () => {
            subSolver.solver.check(constraints, SmtBuildModel() :: subSolver.extraOptions ::: options, name)
          })
      }
    try {
      val firstResult: Option[(CheckRes, Int)] = ConcurrencyUtils.race(results).zipWithIndex.find(!_._1.isUnknown)
      //      firstResult match {
      //        case Some((_, i)) =>
      //        case None =>
      //      }
      firstResult.map(_._1).getOrElse(Unknown())
    } finally {
      // cancel remaining executions
      results.foreach(_.cancel())
    }
  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String = subSolvers.head.solver.exportConstraints(assertions, subSolvers.head.extraOptions ::: options)
}
