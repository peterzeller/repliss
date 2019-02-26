package crdtver.symbolic

/**
  * Simplify expressions that are not directly supported
  * by the underlying smt solver
  */
class Simplifier(ctxt: SymbolicContext) {
  def simp[S <: SymbolicSort](v: SVal[S]): SVal[S] = {
    val simpl = simplifySubsetDomain
    simplify(simpl)(v)
  }

  private type Simplification = PartialFunction[SVal[_ <: SymbolicSort], SVal[_ <: SymbolicSort]]


  private def combine(a: Simplification, b: Simplification): Simplification =
    new PartialFunction[SVal[_<: SymbolicSort], SVal[_<: SymbolicSort]] {
      override def isDefinedAt(x: SVal[_<: SymbolicSort]): Boolean =
        a.isDefinedAt(x) || b.isDefinedAt(x)

      override def apply(v1: SVal[_<: SymbolicSort]): SVal[_<: SymbolicSort] = {
        val v2 = if (a.isDefinedAt(v1)) a(v1) else v1
        if (b.isDefinedAt(v2)) b(v2) else v2
      }
    }

  private def simplifySubsetDomain: Simplification = {
    case IsSubsetOf(s, MapDomain(m)) =>
      val x = ctxt.makeVariable("x")(s.typ.valueSort)
      QuantifierExpr(
        QForall(),
        x,
        SImplies(SSetContains(s, x), SNotEq(SMapGet(m, x), SNone(m.typ.valueSort.valueSort)))
      )
  }

  private def simplify[S <: SymbolicSort](f: Simplification)(v: SVal[S]): SVal[S] = {
    def rec[T <: SymbolicSort](v: SVal[T]): SVal[T] = {
      val simplified1 =
        (v match {
          case c: ConcreteVal[_, _] => v
          case s: SymbolicVariable[T] => v
          case SEq(left, right) =>
            SEq(rec(left), rec(right))
          case SNotEq(left, right) =>
            SNotEq(rec(left), rec(right))
          case SLessThan(left, right) =>
            SLessThan(rec(left), rec(right))
          case SLessThanOrEqual(left, right) =>
            SLessThanOrEqual(rec(left), rec(right))
          case SDistinct(values) =>
            SDistinct(values.map(rec))
          case s: SNone[_] => s
          case SSome(value) =>
            SSome(rec(value))
          case s@SOptionMatch(option, ifSomeVariable, ifSome, ifNone) =>
            s.copy(rec(option), ifSomeVariable, rec(ifSome), rec(ifNone))(s.typ)
          case SReturnVal(methodName, value) =>
            SReturnVal(methodName, rec(value))
          case s: SReturnValNone => s
          case SMapGet(map, key) =>
            SMapGet(rec(map), rec(key))
          case s: SymbolicMapEmpty[_, _, _] => s
          case s@SymbolicMapVar(v) =>
            s.copy(rec(v))(s.concrete, s.keySort, s.valueSort)
          case s@SymbolicMapUpdated(k, v, m) =>
            s.copy(rec(k), rec(v), rec(m))(s.concrete)
          case s@ SymbolicMapUpdatedConcrete(k, n) =>
            s.copy(k, rec(n))(s.concrete, s.keySort, s.valueSort)
          case SSetUnion(a,b) =>
            SSetUnion(rec(a).asInstanceOf[a.type], rec(b).asInstanceOf[a.type])
          case SSetInsert(s, v) =>
            SSetInsert(rec(s).asInstanceOf[s.type], rec(v))
          case s: SSetEmpty[_] => s
          case s@ SSetVar(v) =>
            SSetVar(rec(v))
          case SSetContains(set, value) =>
            SSetContains(rec(set), rec(value))
          case QuantifierExpr(quantifier, variable, body) =>
            QuantifierExpr(quantifier, variable, rec(body))
          case s: SCommitted => s
          case s: SUncommitted => s
          case s: SBool => s
          case SNot(value) =>
            SNot(rec(value))
          case SAnd(left, right) =>
            SAnd(rec(left), rec(right))
          case SOr(left, right) =>
            SOr(rec(left), rec(right))
          case SImplies(left, right) =>
            SImplies(rec(left), rec(right))
          case SFunctionCall(typ, functionName, args) =>
            SFunctionCall(typ, functionName, args.map(rec(_).asInstanceOf[SVal[T]]))
          case SDatatypeValue(inType, constructorName, values, dtyp) =>
            SDatatypeValue(inType, constructorName, values.map(rec(_).asInstanceOf[SVal[T]]), dtyp)
          case SCallInfo(operationName, args) =>
            SCallInfo(operationName, args.map(rec))
          case s: SCallInfoNone => s
          case SInvocationInfo(procname, args) =>
            SInvocationInfo(procname, args.map(rec))
          case s: SInvocationInfoNone => s
          case MapDomain(map) =>
            MapDomain(rec(map))
          case IsSubsetOf(left, right) =>
            IsSubsetOf(rec(left), rec(right))
        }).asInstanceOf[SVal[T]]
      f.applyOrElse(simplified1, (x: SVal[_]) => x).asInstanceOf[SVal[T]]
    }

    rec(v)
  }


}

