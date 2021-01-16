package crdtver.symbolic.smt

import crdtver.symbolic.smt.Solver._
import crdtver.utils.DurationUtils.DurationExt


/**
 * A solver that modifies the timeout of solvers
 */
class TimeoutModSolver(
  solver: Solver,
  factor: Double
) extends Solver {

  override def toString: String = s"$factor$solver"

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    val options2 = options.map {
      case SmtTimeout(t) => SmtTimeout(t * factor)
      case x => x
    }
    solver.check(constraints, options2, name)
  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String =
    solver.exportConstraints(assertions, options)
}
