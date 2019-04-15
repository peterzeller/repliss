package crdtver.symbolic

import crdtver.symbolic.smt.Smt.SmtExpr


abstract class SmtTranslation {

  var datatypeImpl: SortDatatype => SortDatatypeImpl = _

  def translateBool(constraint: SVal[SortBoolean]): SmtExpr

  def translateExpr[T <: SymbolicSort](expr: SVal[T]): SmtExpr

  def parseExpr[T <: SymbolicSort](expr: SmtExpr)(implicit t: T): SVal[T]


  /** exports the given constraints to a format that can be read by the underlying smt solver */
  def exportConstraints(constraints: List[NamedConstraint]): String


}