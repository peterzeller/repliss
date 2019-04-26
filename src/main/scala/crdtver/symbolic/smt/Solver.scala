package crdtver.symbolic.smt

import crdtver.symbolic.{NamedConstraint, SymbolicContext}
import crdtver.symbolic.smt.Smt.SmtExpr

/**
  * Effectively stateless solver.
  *
  * Might use internal caches to improve performance for iterative queries (first calling check(ys)
  * and later check(xs++ys) could be optimized using push and pop on a stateful smt solver in the background)
  *
  */
trait Solver {
  def check(expression: List[Smt.NamedConstraint]): CheckRes
  def exportConstraints(assertions: List[Smt.NamedConstraint]): String

  sealed abstract class CheckRes()

  abstract class Satisfiable() extends CheckRes() {
    def getModel: Model
  }

  case class Unknown() extends CheckRes()

  case class Unsatisfiable() extends CheckRes()

  trait Model {
    def eval(expr: SmtExpr, bool: Boolean): SmtExpr
  }

}

