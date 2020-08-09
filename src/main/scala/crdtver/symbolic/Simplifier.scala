package crdtver.symbolic

import scala.collection.immutable.Set
import scala.collection.mutable

/**
  * Simplify expressions that are not directly supported
  * by the underlying smt solver
  */
class Simplifier(ctxt: SymbolicContext) {

  import Simplifier._

  def simp[S <: SymbolicSort](v: SVal[S]): SVal[S] = {
    val simpl = simplifySubsetDomain
    rewrite(simpl)(v)
  }

  private def combine(a: Simplification, b: Simplification): Simplification =
    new PartialFunction[SVal[_ <: SymbolicSort], SVal[_ <: SymbolicSort]] {
      override def isDefinedAt(x: SVal[_ <: SymbolicSort]): Boolean =
        a.isDefinedAt(x) || b.isDefinedAt(x)

      override def apply(v1: SVal[_ <: SymbolicSort]): SVal[_ <: SymbolicSort] = {
        val v2 = if (a.isDefinedAt(v1)) a(v1) else v1
        if (b.isDefinedAt(v2)) b(v2) else v2
      }
    }

  private def simplifySubsetDomain: Simplification = {
    case IsSubsetOf(s, MapDomain(m)) =>
      val x = ctxt.makeBoundVariable("x")(s.typ.valueSort)
      QuantifierExpr(
        QForall(),
        x,
        SImplies(SSetContains(s, x), SNotEq(SMapGet(m, x), SNone(m.typ.valueSort.valueSort)))
      )
  }
}

object Simplifier {
  final type Simplification = PartialFunction[SVal[_ <: SymbolicSort], SVal[_ <: SymbolicSort]]

  def rewrite[S <: SymbolicSort](f: Simplification)(v: SVal[S]): SVal[S] = {
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
          case s: SymbolicMapEmpty[_, _] => s
          case s@SymbolicMapUpdated(k, v, m) =>
            s.copy(rec(k), rec(v), rec(m))
          case SSetUnion(a, b) =>
            SSetUnion(rec(a).asInstanceOf[a.type], rec(b).asInstanceOf[a.type])
          case SSetInsert(s, vs) =>
            SSetInsert(rec(s).asInstanceOf[s.type], vs.map(rec))
          case s: SSetEmpty[_] => s
          case s@SSetVar(v) =>
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
          case s: SValOpaque[t] => s
          case SNamedVal(name, v) => SNamedVal(name, rec(v))
          case SChooseSome(condition, value) =>
            SChooseSome(rec(condition), value)(value.typ)
        }).asInstanceOf[SVal[T]]
      f.applyOrElse(simplified1, (x: SVal[_]) => x).asInstanceOf[SVal[T]]
    }

    rec(v)
  }


  /**
    * extracts named values and puts them into new constraints
    */
  def extractNamedValues(constraints1: List[NamedConstraint]): List[NamedConstraint] = {
    var usedVarnames: Set[String] = findUsedVariables(constraints1)
    var extracted = Map[SNamedVal[_ <: SymbolicSort], SymbolicVariable[_ <: SymbolicSort]]()
    val result = mutable.ListBuffer[NamedConstraint]()


    def makeVar[T <: SymbolicSort](name: String, t: T): SymbolicVariable[T] = {
      var name2 = name
      var i = 0
      while (usedVarnames.contains(name2)) {
        i += 1
        name2 = name + i
      }
      usedVarnames += name2
      SymbolicVariable(name2, false, t)
    }

    def extr(c: NamedConstraint): Unit = {
      val e2 = Simplifier.rewrite({
        case nv@SNamedVal(name, value) =>
          extracted.get(nv) match {
            case Some(v) =>
              v
            case None =>
              val vfreeVars: List[SymbolicVariable[_ <: SymbolicSort]] = freeVars(value, Set()).toList
              var t: SymbolicSort = value.typ
              for (fv <- vfreeVars.reverse) {
                t = SortMap(fv.typ, t)
              }
              val v = makeVar(name, t)
              extracted = extracted + (nv -> v)

              var left: SVal[SymbolicSort] = v
              for (fv <- vfreeVars) {
                left = SMapGet(left.castUnsafe[SortMap[SymbolicSort, SymbolicSort]], fv.castUnsafe[SymbolicSort])
              }

              result += NamedConstraint(s"${nv.name}_def", c.priority, SVal.forallL(vfreeVars, SEq(left, value.castUnsafe[SymbolicSort])))
              left
          }
      })(c.constraint)
      result += NamedConstraint(c.description, c.priority, e2)
    }

    for (c <- constraints1)
      extr(c)
    return result.toList

  }

  def freeVars(value: SVal[_ <: SymbolicSort], bound: Set[SymbolicVariable[_ <: SymbolicSort]]): Set[SymbolicVariable[_ <: SymbolicSort]] = {
    value match {
      case v@SymbolicVariable(name, isBound, typ) =>
        if (isBound && !bound.contains(v))
          Set(v)
        else
          Set()
      case QuantifierExpr(quantifier, variable, body) =>
        freeVars(body, bound + variable)
      case _ =>
        value.children.flatMap(freeVars(_, bound)).toSet
    }
  }


  /**
    * eliminates nested SChooseSome values
    *
    * TODO this is not always safe since the context changes,
    * ultimately queries should be translated differently, since
    * this SChooseSome is nondeterministic and as such not compatible with logic
    */
  def flattenConstraints(constraints1: List[NamedConstraint]): List[NamedConstraint] = {
    val result = mutable.ListBuffer[NamedConstraint]()

    def extr(c: NamedConstraint): Unit = {
      val e2 = Simplifier.rewrite({
        case SChooseSome(constraint, variable) =>
          extr(NamedConstraint(s"choose_${variable.name}", c.priority, constraint))
          variable
      })(c.constraint)
      result += NamedConstraint(c.description, c.priority, e2)
    }

    for (c <- constraints1)
      extr(c)
    result.toList
  }


  private def findUsedVariables(constraints1: List[NamedConstraint]): Set[String] = {
    var usedVarnames: Set[String] = Set()
    for (c <- constraints1) {
      Simplifier.rewrite({
        case v@SymbolicVariable(name, _, t) =>
          usedVarnames += name
          v
      })(c.constraint)
    }
    usedVarnames
  }
}

