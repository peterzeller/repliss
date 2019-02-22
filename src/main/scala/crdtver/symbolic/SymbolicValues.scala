package crdtver.symbolic

import crdtver.language.InputAst.InTypeExpr
import crdtver.utils.PrettyPrintDoc
import crdtver.utils.PrettyPrintDoc.sep

import scala.language.existentials

/** a symbolic value
  *
  * the type parameter T is the sort of the value.
  * It is a phantom type (only used for type checking in the DSL)
  *
  * */
sealed abstract class SVal[T <: SymbolicSort] {
  def upcast(): SVal[SymbolicSort] = this.asInstanceOf[SVal[SymbolicSort]]

  def typ: T

  def ===(other: SVal[T]): SVal[SortBoolean] = SEq(this, other)

  def !==(other: SVal[T]): SVal[SortBoolean] = SNotEq(this, other)

  def children: List[SVal[_ <: SymbolicSort]] =
    childrenP._1

  def childrenT: List[SymbolicSort] =
      childrenP._2

  def childrenP: (List[SVal[_ <: SymbolicSort]], List[SymbolicSort]) = this match {
    case ConcreteVal(_) =>
      (List(), List())
    case SymbolicVariable(_, typ) =>
      (List(), List(typ))
    case SEq(left, right) =>
      (List(left, right), List())
    case SNotEq(left, right) =>
      (List(left, right), List())
    case SLessThan(left, right) =>
      (List(left, right), List())
    case SLessThanOrEqual(left, right) =>
      (List(left, right), List())
    case SDistinct(values) =>
      (values, List())
    case SNone(ofTyp) =>
      (List(), List(ofTyp))
    case SSome(value) =>
      (List(value), List())
    case SOptionMatch(option, ifSomeVariable, ifSome, ifNone) =>
      (List(option, ifSome, ifNone).asInstanceOf, List(ifSomeVariable.typ))
    case SReturnVal(methodName, value) =>
      (List(value), List())
    case SReturnValNone() =>
      (List(), List())
    case SMapGet(map, key) =>
      (List(map,key), List())
    case value: SymbolicMap[_, _] =>
      value match {
        case SymbolicMapEmpty(d) =>
          (List(d), List())
        case SymbolicMapVar(v) =>
          (List(v), List())
        case SymbolicMapUpdated(k,v,b) =>
          (List(k,v,b), List())
        case SymbolicMapUpdatedConcrete(k,b) =>
          ((k.values ++ List(b)).asInstanceOf, List())
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetUnion(a, b) =>
          (List(a,b), List())
        case SSetInsert(a, b) =>
          (List(a,b), List())
        case SSetEmpty() =>
          (List(), List())
        case SSetVar(v) =>
          (List(v), List())
      }
    case SSetContains(set, value) =>
      (List(set, value), List())
    case QuantifierExpr(quantifier, variable, body) =>
      (List(body), List(variable.typ))
    case SCommitted() =>
      (List(), List())
    case SUncommitted() =>
      (List(), List())
    case SBool(value) =>
      (List(), List())
    case SNot(value) =>
      (List(value), List())
    case SAnd(left, right) =>
      (List(left, right), List())
    case SOr(left, right) =>
      (List(left, right), List())
    case SImplies(left, right) =>
      (List(left, right), List())
    case SFunctionCall(typ, functionName, args) =>
      (args, List())
    case SDatatypeValue(inType, constructorName, values, dtyp) =>
      (values, List(dtyp))
    case SCallInfo(operationName, args) =>
      (args, List())
    case SCallInfoNone() =>
      (List(), List())
    case SInvocationInfo(procname, args) =>
      (args, List())
    case SInvocationInfoNone() =>
      (List(), List())
    case MapDomain(map) =>
      (List(map), List())
    case IsSubsetOf(left, right) =>
      (List(left, right), List())
  }

  def prettyPrint: PrettyPrintDoc.Doc = {
    import PrettyPrintDoc._

    def printOp(l: SVal[_], op: String, r: SVal[_]): Doc =
      ("(" <> l.prettyPrint <+> op <+> r.prettyPrint <> ")") :<|>
        (() => "(" <> l.prettyPrint <+> line <> op <+> r.prettyPrint <> ")")

    this.asInstanceOf[SVal[_]] match {
      case ConcreteVal(value) =>
        value.toString
      case SymbolicVariable(name, typ) =>
        name
      case SEq(left, right) =>
        printOp(left, "==", right)
      case SNotEq(left, right) =>
        printOp(left, "!=", right)
      case SLessThan(left, right) =>
        printOp(left, "<", right)
      case SLessThanOrEqual(left, right) =>
        printOp(left, "<=", right)
      case SNone(t) =>
        s"None<$t>"
      case SSome(value) =>
        "Some(" <> value.prettyPrint <> ")"
      case SOptionMatch(option, ifSomeVariable, ifSome, ifNone) =>
        group("(match " <> option.prettyPrint <> " with" </>
          nested(2, "| None => " <> ifNone.prettyPrint) </>
          nested(2, "| Some(" <> ifSomeVariable.prettyPrint <> ") => " <> ifSome.prettyPrint) <> ")")
      case SReturnVal(methodName, value) =>
        "(return " <> methodName <+> value.prettyPrint <> ")"
      case SMapGet(map, key) =>
        map.prettyPrint <> "[" <> key.prettyPrint <> "]"
      case value: SymbolicMap[_, _] =>
        value match {
          case SymbolicMapVar(v) =>
            v.prettyPrint
          case SymbolicMapEmpty(defaultValue) =>
            "empty"
          case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
            baseMap.prettyPrint <> "(" <> updatedKey.prettyPrint <+> ":=" <+> newValue.prettyPrint <> ")"
          case SymbolicMapUpdatedConcrete(currentKnowledge, baseMap) =>
            baseMap.prettyPrint <> "(" <> currentKnowledge.toString() <> ")"
        }
      case value: SymbolicSet[_] =>
        value match {
          case SSetVar(v) =>
            v.prettyPrint
          case SSetEmpty() =>
            "{}"
          case SSetInsert(set, v) =>
            "(" <> set.prettyPrint <> " ∪ {" <> v.prettyPrint <> "})"
          case SSetUnion(a, b) =>
            "(" <> a.prettyPrint <> " ∪ " <> b.prettyPrint <> ")"
        }
      case SSetContains(set, value) =>
        printOp(set, "contains", value)
      case QuantifierExpr(quantifier, variable, body) =>
        (quantifier.toString <> variable.name <> ":" <+> variable.typ.toString <+> "::" <+> body.prettyPrint) :<|>
          (() => quantifier.toString <> variable.name <> ":" <+> variable.typ.toString <+> "::" </> nested(2, body.prettyPrint))
      case SCommitted() =>
        "committed"
      case SUncommitted() =>
        "uncommitted"
      case SBool(value) =>
        value.toString
      case SNot(value) =>
        "!" <> value.prettyPrint
      case SAnd(left, right) =>
        printOp(left, "&&", right)
      case SOr(left, right) =>
        printOp(left, "||", right)
      case SImplies(left, right) =>
        printOp(left, "==>", right)
      case SFunctionCall(typ, functionName, args) =>
        functionName <> "(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SDatatypeValue(inType, constructorName, values, _) =>
        constructorName <> "(" <> sep(",", values.map(_.prettyPrint)) <> ")"
      case SInvocationInfo(procname, args) =>
        procname <> "(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SInvocationInfoNone() =>
        "no_invocation"
      case SReturnValNone() =>
        "not_returned"
      case MapDomain(map) =>
        "dom(" <> map.prettyPrint <> ")"
      case IsSubsetOf(left, right) =>
        printOp(left, "⊆", right)
      case SDistinct(args) =>
        "distinct(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SCallInfo(op, args) =>
        op <> "(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SCallInfoNone() =>
        "no_call"
    }
  }

  def defaultToString: String = super.toString

  override def toString: String =
    prettyPrint.prettyStr(120)

}

object SVal {
  def and(exprs: List[SVal[SortBoolean]]): SVal[SortBoolean] = exprs match {
    case List() => SBool(true)
    case _ => exprs.reduce(SAnd)
  }

  def forall[T <: SymbolicSort](variable: SymbolicVariable[T], body: SVal[SortBoolean]): QuantifierExpr =
    QuantifierExpr(QForall(), variable, body)

  def exists[T <: SymbolicSort](variable: SymbolicVariable[T], body: SVal[SortBoolean]): QuantifierExpr =
    QuantifierExpr(QExists(), variable, body)

  def datatype(typ: InTypeExpr, name: String, t: SortDatatype, args: SVal[SortValue]*)(implicit ctxt: SymbolicContext): SVal[SortDatatype] =
    SDatatypeValue(ctxt.translateSortDatatypeToImpl(typ), name, args.toList, t)

  def datatype(typ: InTypeExpr, name: String, t: SortDatatype, args: List[SVal[SortValue]])(implicit ctxt: SymbolicContext): SVal[SortDatatype] =
    SDatatypeValue(ctxt.translateSortDatatypeToImpl(typ), name, args,t )

  implicit class MapGetExtension[K <: SymbolicSort, V <: SymbolicSort](mapExpr: SVal[SortMap[K, V]]) {
    def apply(key: SVal[K]): SMapGet[K, V] = SMapGet(mapExpr, key)
  }

}


case class ConcreteVal[R, T <: SymbolicSort](value: R)(implicit val typ: T) extends SVal[T] {
}


case class SymbolicVariable[Sort <: SymbolicSort](
  name: String,
  typ: Sort
) extends SVal[Sort] {
  override def toString: String = s"$name: $typ"
}

case class SEq[T <: SymbolicSort](left: SVal[T], right: SVal[T]) extends SVal[SortBoolean] {
  require(left.typ == right.typ, s"Incompatible types in $left == $right:  ${left.typ} and ${right.typ}")

  override def typ: SortBoolean = SortBoolean()
}

case class SNotEq[T <: SymbolicSort](left: SVal[T], right: SVal[T]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SLessThan(left: SVal[SortInt], right: SVal[SortInt]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SLessThanOrEqual(left: SVal[SortInt], right: SVal[SortInt]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SDistinct[T <: SymbolicSort](values: List[SVal[T]]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}


case class SNone[T <: SymbolicSort](ofTyp: T) extends SVal[SortOption[T]] {
  override def typ: SortOption[T] = SortOption(ofTyp)
}

case class SSome[T <: SymbolicSort](value: SVal[T]) extends SVal[SortOption[T]] {
  override def typ: SortOption[T] = SortOption(value.typ)
}

case class SOptionMatch[O <: SymbolicSort, T <: SymbolicSort](
  option: SVal[SortOption[O]],
  ifSomeVariable: SymbolicVariable[O],
  ifSome: SVal[T],
  ifNone: SVal[T]
)(implicit val typ: T) extends SVal[T] {
}

case class SReturnVal(methodName: String, value: SVal[SortValue]) extends SVal[SortInvocationRes] {
  override def typ: SortInvocationRes = SortInvocationRes()
}

case class SReturnValNone() extends SVal[SortInvocationRes] {
  override def typ: SortInvocationRes = SortInvocationRes()
}


// TODO could make application more generic and use Hlists (whooo!)
// case class SApp(func: SFunc, args: List[SVal[_]])


case class SMapGet[K <: SymbolicSort, V <: SymbolicSort](map: SVal[SortMap[K, V]], key: SVal[K]) extends SVal[V] {
  override def typ: V = map.typ.valueSort
}


// Map with concrete values
sealed abstract class SymbolicMap[K <: SymbolicSort, V <: SymbolicSort] extends SVal[SortMap[K, V]] {

  type KRep >: Nothing

  override def typ: SortMap[K, V]

  def concrete: SymbolicSortConcrete[K, KRep]

  def put(key: SVal[K], value: SVal[V]): SymbolicMap[K, V] = {
    key match {
      case cv: ConcreteVal[KRep, K]@unchecked =>
        val v: KRep = concrete.getValue(cv)
        val currentKnowledge: Map[KRep, SVal[V]] = Map((v, value))
        SymbolicMapUpdatedConcrete[K, V, KRep](currentKnowledge, this)(concrete, typ.keySort, typ.valueSort)
      case _ => SymbolicMapUpdated[K, V, KRep](
        updatedKey = key,
        newValue = value,
        baseMap = this
      )(concrete)
    }
  }


  def get(key: SVal[K]): SVal[V] =
    SMapGet(this, key)

}

case class SymbolicMapVar[K <: SymbolicSort, V <: SymbolicSort, KR](variable: SVal[SortMap[K, V]])
  (implicit val concrete: SymbolicSortConcrete[K, KR], keySort: K, valueSort: V)
  extends SymbolicMap[K, V] {
  override type KRep = KR

  override def typ: SortMap[K, V] = SortMap(keySort, valueSort)
}

object SymbolicMapVar {
  def symbolicMapVar[K <: SymbolicSort, V <: SymbolicSort](name: String)
    (implicit keySort: K, valueSort: V, ctxt: SymbolicContext): SymbolicMap[K, V] =
    SymbolicMapVar[K, V, Nothing](ctxt.makeVariable[SortMap[K, V]](name))(SymbolicSortConcrete.default, keySort, valueSort)
}


case class SymbolicMapEmpty[K <: SymbolicSort, V <: SymbolicSort, KR](
  defaultValue: SVal[V]
)(implicit val concrete: SymbolicSortConcrete[K, KR], keySort: K, valueSort: V) extends SymbolicMap[K, V] {
  override type KRep = KR

  override def typ: SortMap[K, V] = SortMap(keySort, valueSort)
}

// map updated with a symbolic key
case class SymbolicMapUpdated[K <: SymbolicSort, V <: SymbolicSort, KR](
  updatedKey: SVal[K],
  newValue: SVal[V],
  baseMap: SVal[SortMap[K, V]]
)(implicit val concrete: SymbolicSortConcrete[K, KR])
  extends SymbolicMap[K, V] {
  override type KRep = KR

  override def typ: SortMap[K, V] = SortMap(updatedKey.typ, newValue.typ)
}


// map updated with concrete keys
// (optimization suggested by Symbooglix paper)
case class SymbolicMapUpdatedConcrete[K <: SymbolicSort, V <: SymbolicSort, KR](
  currentKnowledge: Map[KR, SVal[V]],
  baseMap: SVal[SortMap[K, V]]
)(implicit val concrete: SymbolicSortConcrete[K, KR], keySort: K, valueSort: V) extends SymbolicMap[K, V] {

  override type KRep = KR

  override def typ: SortMap[K, V] = SortMap(keySort, valueSort)

  override def put(key: SVal[K], value: SVal[V]): SymbolicMap[K, V] = {
    key match {
      case cv: ConcreteVal[KRep, K]@unchecked =>
        copy(
          currentKnowledge = currentKnowledge + (concrete.getValue(cv) -> value)
        )
      case _ => SymbolicMapUpdated(
        updatedKey = key,
        newValue = value,
        baseMap = this
      )
    }

  }

  override def get(key: SVal[K]): SVal[V] = {

    key match {
      case cv: ConcreteVal[KRep, K]@unchecked =>
        currentKnowledge.get(concrete.getValue(cv)) match {
          case Some(value) =>
            return value
          case None =>
            super.get(key)
        }
      case _ =>
        super.get(key)
    }
  }

  def toSymbolicUpdates(): SymbolicMap[K, V] = {
    var res: SymbolicMap[K, V] = SymbolicMapVar(baseMap)
    for ((k, v) <- currentKnowledge) {
      res = SymbolicMapUpdated(concrete.makeValue(k), v, res)
    }
    res
  }

}


sealed abstract class SymbolicSet[T <: SymbolicSort] extends SVal[SortSet[T]] {

  def isSubsetOf(other: SVal[SortSet[T]]): SVal[SortBoolean] = {
    IsSubsetOf(this, other)
  }

  def contains(value: SVal[T]): SVal[SortBoolean] = {
    SSetContains(this, value)
  }

  override def typ: SortSet[T]

}

case class SSetVar[T <: SymbolicSort](variable: SVal[SortSet[T]]) extends SymbolicSet[T] {
  override def typ: SortSet[T] = variable.typ
}

case class SSetEmpty[T <: SymbolicSort]()(implicit val t: T) extends SymbolicSet[T] {
  override def typ: SortSet[T] = SortSet(t)
}

case class SSetInsert[T <: SymbolicSort](set: SymbolicSet[T], value: SVal[T]) extends SymbolicSet[T] {
  override def typ: SortSet[T] = set.typ
}


case class SSetUnion[T <: SymbolicSort](set1: SymbolicSet[T], set2: SymbolicSet[T]) extends SymbolicSet[T] {
  override def typ: SortSet[T] = set1.typ
}

case class SSetContains[T <: SymbolicSort](set: SVal[SortSet[T]], value: SVal[T]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}


sealed abstract class Quantifier

case class QForall() extends Quantifier {
  override def toString: String = "∀"
}

case class QExists() extends Quantifier {
  override def toString: String = "∃"
}


case class QuantifierExpr(
  quantifier: Quantifier,
  variable: SymbolicVariable[_ <: SymbolicSort],
  body: SVal[SortBoolean]
) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SCommitted() extends SVal[SortTransactionStatus] {
  override def typ: SortTransactionStatus = SortTransactionStatus()
}

case class SUncommitted() extends SVal[SortTransactionStatus] {
  override def typ: SortTransactionStatus = SortTransactionStatus()
}


case class SBool(value: Boolean) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SNot(value: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SAnd(left: SVal[SortBoolean], right: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SOr(left: SVal[SortBoolean], right: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SImplies(left: SVal[SortBoolean], right: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}


case class SFunctionCall[T <: SymbolicSort](typ: T, functionName: String, args: List[SVal[_ <: SymbolicSort]])
  extends SVal[T] {
}

case class SDatatypeValue(inType: SortDatatypeImpl, constructorName: String, values: List[SVal[_ <: SymbolicSort]], dtyp: SortDatatype) extends SVal[SortDatatype] {
  override def typ: SortDatatype = dtyp
}

case class SCallInfo(operationName: String, args: List[SVal[SymbolicSort]]) extends SVal[SortCall] {
  override def typ: SortCall = SortCall()
}

case class SCallInfoNone() extends SVal[SortCall] {
  override def typ: SortCall = SortCall()
}


case class SInvocationInfo(procname: String, args: List[SVal[SortValue]]) extends SVal[SortInvocationInfo] {
  override def typ: SortInvocationInfo = SortInvocationInfo()
}

case class SInvocationInfoNone() extends SVal[SortInvocationInfo] {
  override def typ: SortInvocationInfo = SortInvocationInfo()
}

case class MapDomain[K <: SymbolicSort, V <: SymbolicSort](map: SVal[SortMap[K, SortOption[V]]]) extends SVal[SortSet[K]] {
  override def typ: SortSet[K] = SortSet(map.typ.keySort)
}

case class IsSubsetOf[T <: SymbolicSort](left: SVal[SortSet[T]], right: SVal[SortSet[T]]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}