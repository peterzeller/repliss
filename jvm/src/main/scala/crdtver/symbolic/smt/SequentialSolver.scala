package crdtver.symbolic.smt

import crdtver.symbolic.smt.Solver._
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.{ConcurrencyUtils, TimeTaker}


/**
 * Runs the given solvers in sequence, returning the first non-unknown result
 */
class SequentialSolver(
  subSolvers: List[Solver]
) extends Solver {

  override def toString: String = s"(${subSolvers.mkString(";")})"

  require(subSolvers.nonEmpty)

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    for (s <- subSolvers) {
      val r = s.check(constraints, options, name)
      if (r != Unknown())
        return r
    }
    Unknown()
  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String = subSolvers.head.exportConstraints(assertions, options)
}
