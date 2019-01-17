package crdtver.symbolic

import com.microsoft.z3._

/**
  *
  */
object Z3Translation {
  def translateBool(expr: SVal[SortBoolean])(implicit ctxt: Context): BoolExpr = {
    translateExpr(expr).asInstanceOf[BoolExpr]
  }

  def translateExpr[T <: SymbolicSort](expr: SVal[T])(implicit ctxt: Context): Expr = expr match {
    case ConcreteVal(value) =>
      value match {
        case b: Boolean => ctxt.mkBool(b)
        case i: BigInt => ctxt.mkInt(i.toString())
        case _ =>
          throw new RuntimeException(s"unhandled concrete value $value (${value.getClass})")
      }
    case SymbolicVariable(name, typ) =>
      ctxt.mkBoolConst(name)
    case SEq(left, right) =>
      ctxt.mkEq(translateExpr(left), translateExpr(right))
    case SNotEq(left, right) =>
      ctxt.mkNot(ctxt.mkEq(translateExpr(left), translateExpr(right)))
    case SNone() =>
      ???
    case SSome(value) =>
      ???
    case SMapGet(map, key) =>
      ???
    case value: SymbolicMap[_, _, _] =>
      ???
    case value: SymbolicSet[_] =>
      ???
    case SSetContains(set, value) =>
      ???
    case QuantifierExpr(quantifier, variable, body) =>
      ???
    case Committed() =>
      ???
    case Uncommitted() =>
      ???
    case SBool(value) =>
      ???
    case SNot(value) =>
      ???
    case SAnd(left, right) =>
      ???
    case SOr(left, right) =>
      ???
    case SImplies(left, right) =>
      ???
    case SDatatypeValue(constructorName, values) =>
      ???
    case SInvocationInfo(procname, args) =>
      ???
    case MapDomain(map) =>
      ???
    case IsSubsetOf(left, right) =>
      ???
  }

}
