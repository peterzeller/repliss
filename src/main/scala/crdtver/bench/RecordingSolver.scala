package crdtver.bench

import java.time.Duration

import codes.reactive.scalatime._
import crdtver.symbolic.smt.Solver._
import crdtver.symbolic.smt._
import crdtver.utils.ListExtensions.ListUtils

import scala.collection.mutable.ListBuffer
import scala.math.Ordered.orderingToOrdered

/**
 * A solver for benchmarks.
 * Does not actually solve anything, just records the requests
 */
object RecordingSolver extends Solver {



  override def toString: String = s"RecordingSolver"

  var currentInput = ""

  case class Check(
    input: String,
    name: String,
    constraints: List[Smt.NamedConstraint],
    isTiny: Boolean,
  )

  def getChecks: List[Check] = checks.toList

  private val checks = new ListBuffer[Check]

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    val timeOut: Duration = options.extract { case SmtTimeout(t) => t }.getOrElse(2.minutes)

    val isTiny = timeOut < 1.minutes
    checks.addOne(Check(
      currentInput,
      name,
      constraints,
      isTiny
    ))

    if (isTiny) {
      Unknown()
    } else {
      Unsatisfiable(constraints)
    }

  }


  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String = SmtLibPrinter.print(assertions).prettyStr(120)
}
