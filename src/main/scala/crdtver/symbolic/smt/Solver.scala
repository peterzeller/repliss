package crdtver.symbolic.smt

import crdtver.symbolic.{NamedConstraint, SymbolicContext}
import crdtver.symbolic.smt.Smt.SmtExpr

import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
  * Effectively stateless solver.
  *
  * Might use internal caches to improve performance for iterative queries (first calling check(ys)
  * and later check(xs++ys) could be optimized using push and pop on a stateful smt solver in the background)
  *
  */
trait Solver {
  def check(expression: List[Smt.NamedConstraint], options: List[SmtOption] = List(), name: String): CheckRes
  def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption] = List()): String

  sealed abstract class CheckRes()

  abstract class Satisfiable() extends CheckRes() {
    def getModel: Model
  }

  case class Unknown() extends CheckRes()

  case class Unsatisfiable(
    unsatCore: List[Smt.NamedConstraint]
  ) extends CheckRes()

  trait Model {
    def eval(expr: SmtExpr, bool: Boolean): SmtExpr
  }
}

abstract class SmtOption {

}

case class FiniteModelFind() extends SmtOption

case class SmtTimeout(duration: Duration) extends SmtOption

case class ResourceLimit(limit: Int) extends SmtOption

case class SmtBuildModel() extends SmtOption

case class SmtBuildUnsatCore() extends SmtOption