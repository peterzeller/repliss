package crdtver.symbolic

import com.microsoft.z3._

/**
  *
  */
object Z3Translation {
  def translateBool(expr: SVal[SortBoolean])(implicit ctxt: Context): BoolExpr = expr match {
    case _ => ???
  }

}
