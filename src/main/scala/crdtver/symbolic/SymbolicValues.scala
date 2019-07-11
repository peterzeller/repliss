package crdtver.symbolic

import com.microsoft.z3.Expr
import crdtver.language.TypedAst.InTypeExpr
import crdtver.language.crdts.UniqueName
import crdtver.utils.PrettyPrintDoc
import crdtver.utils.PrettyPrintDoc.sep
import edu.nyu.acsys.CVC4.Kind

import scala.language.existentials

/** a symbolic value
  *
  * the type parameter T is the sort of the value.
  * It is a phantom type (only used for type checking in the DSL)
  *
  * */
sealed abstract class SVal[T <: SymbolicSort] {
  def upcast[S >: T <: SymbolicSort]: SVal[S] = this.asInstanceOf[SVal[S]]

  def cast[S <: SymbolicSort]: SVal[S] = this.asInstanceOf[SVal[S]]

  def typ: T

  def ===(other: SVal[T]): SVal[SortBoolean] = SEq(this, other)

  def !==(other: SVal[T]): SVal[SortBoolean] = SNotEq(this, other)

  def children: List[SVal[_ <: SymbolicSort]] =
    childrenP._1

  def childrenT: List[SymbolicSort] =
    childrenP._2

  def childrenP: (List[SVal[_ <: SymbolicSort]], List[SymbolicSort]) = this.asInstanceOf[SVal[_]] match {
    case ConcreteVal(_) =>
      (List(), List())
    case SymbolicVariable(_, _, typ) =>
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
      (List(option, ifSome, ifNone).asInstanceOf[List[SVal[_ <: SymbolicSort]]], List(ifSomeVariable.typ))
    case SReturnVal(methodName, value) =>
      (List(value), List())
    case SReturnValNone() =>
      (List(), List())
    case SMapGet(map, key) =>
      (List(map, key), List())
    case value: SymbolicMap[_, _] =>
      value match {
        case SymbolicMapEmpty(d) =>
          (List(d), List())
        case SymbolicMapVar(v) =>
          (List(v), List())
        case SymbolicMapUpdated(k, v, b) =>
          (List(k, v, b), List())
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetUnion(a, b) =>
          (List(a, b), List())
        case SSetInsert(a, b) =>
          (List(a) ++ b.toList, List())
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
    case SCallInfo(arg) =>
      (List(arg), List())
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
    case SValOpaque(k, v, t) =>
      (List(), List(t))
    case SNamedVal(_, v) => (List(v), List())
  }

  def prettyPrint: PrettyPrintDoc.Doc = {
    import PrettyPrintDoc._

    def printOp(l: SVal[_], op: String, r: SVal[_]): Doc =
      ("(" <> l.prettyPrint <+> op <+> r.prettyPrint <> ")") :<|>
        (() => "(" <> l.prettyPrint <+> line <> op <+> r.prettyPrint <> ")")

    this.asInstanceOf[SVal[_]] match {
      case ConcreteVal(value) =>
        value.toString
      case SymbolicVariable(name, _, typ) =>
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
        "(return " <> methodName.toString <+> value.prettyPrint <> ")"
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
        }
      case value: SymbolicSet[_] =>
        value match {
          case SSetVar(v) =>
            v.prettyPrint
          case SSetEmpty() =>
            "{}"
          case SSetInsert(SSetEmpty(), v) =>
            "{" <> sep(", ", v.toList.map(_.prettyPrint)) <> "})"
          case SSetInsert(set, v) =>
            "(" <> set.prettyPrint <> " ∪ {" <> sep(", ", v.toList.map(_.prettyPrint)) <> "})"
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
      case SCallInfo(operation) =>
        "call(" <> operation.prettyPrint <> ")"
      case SCallInfoNone() =>
        "no_call"
      case SValOpaque(k, v, t) =>
        //        s"OPAQUE($k, $v, $t)"
        v.toString
      case SNamedVal(name, v) =>
        "(*" <+> name <+> "*) " <> v.prettyPrint
    }
  }

  def defaultToString: String = super.toString

  override def toString: String =
    prettyPrint.prettyStr(120)

}

object SVal {
  def makeSet[T <: SymbolicSort](values: Iterable[SVal[T]])(implicit t: T): SVal[SortSet[T]] =
    SSetInsert(SSetEmpty[T](), values.toSet)

  def and(exprs: List[SVal[SortBoolean]]): SVal[SortBoolean] = exprs match {
    case List() => SBool(true)
    case _ => exprs.reduce(SAnd)
  }

  def forall[T <: SymbolicSort](variable: SymbolicVariable[T], body: SVal[SortBoolean]): QuantifierExpr =
    QuantifierExpr(QForall(), variable, body)

  def forallL(variables: List[SymbolicVariable[_ <: SymbolicSort]], body: SVal[SortBoolean]): SVal[SortBoolean] =
    variables.foldRight(body)(forall(_, _))

  def exists[T <: SymbolicSort](variable: SymbolicVariable[T], body: SVal[SortBoolean]): QuantifierExpr =
    QuantifierExpr(QExists(), variable, body)

  def datatype(typ: InTypeExpr, name: String, t: SortDatatype, args: SVal[SortValue]*)(implicit ctxt: SymbolicContext): SVal[SortDatatype] =
    SDatatypeValue(ctxt.translateSortDatatypeToImpl(typ), name, args.toList, t)

  def datatype(typ: InTypeExpr, name: String, t: SortDatatype, args: List[SVal[SortValue]])(implicit ctxt: SymbolicContext): SVal[SortDatatype] =
    SDatatypeValue(ctxt.translateSortDatatypeToImpl(typ), name, args, t)

  implicit class MapGetExtension[K <: SymbolicSort, V <: SymbolicSort](mapExpr: SVal[SortMap[K, V]]) {
    def apply(key: SVal[K]): SMapGet[K, V] = SMapGet(mapExpr, key)
  }

  implicit class BooleanSValExtensions(left: SVal[SortBoolean]) {
    def -->(right: SVal[SortBoolean]): SVal[SortBoolean] = SImplies(left, right)

    def <-->(right: SVal[SortBoolean]): SVal[SortBoolean] = SEq(left, right)

    def &&(right: SVal[SortBoolean]): SVal[SortBoolean] = SAnd(left, right)

    def ||(right: SVal[SortBoolean]): SVal[SortBoolean] = SOr(left, right)

    def unary_!(): SVal[SortBoolean] = SNot(left)

  }

  implicit class SetSValExtensions[T <: SymbolicSort](left: SVal[SortSet[T]]) {
    def contains(right: SVal[T]): SVal[SortBoolean] = SSetContains(left, right)
  }


  implicit class CallExtensions(left: SVal[SortCallId]) {
    def happensBefore(right: SVal[SortCallId])(implicit state: SymbolicState): SVal[SortBoolean] =
      ExprTranslation.callHappensBefore(left, right)

    /** set of calls that happened before this one */
    def happensBeforeSet(implicit state: SymbolicState): SVal[SortSet[SortCallId]] =
      state.happensBefore.get(left)

    def inSameTransactionAs(right: SVal[SortCallId])(implicit state: SymbolicState): SVal[SortBoolean] =
      state.callOrigin.get(left) === state.callOrigin.get(right)

    def op(implicit state: SymbolicState): SVal[SortCall] =
      state.calls.get(left)

    def isVisible(implicit state: SymbolicState): SVal[SortBoolean] =
      state.visibleCalls.contains(left)


    def tx(implicit state: SymbolicState): SVal[SortOption[SortTxId]] =
      state.callOrigin.get(left)
  }

  implicit class TransactionExtensions(left: SVal[SortTxId]) {
    def invocation(implicit state: SymbolicState): SVal[SortOption[SortInvocationId]] =
      state.transactionOrigin.get(left)

  }

  implicit class InvocationExtensions(left: SVal[SortInvocationId]) {
    def happensBefore(right: SVal[SortInvocationId])(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SortBoolean] =
      ExprTranslation.invocationHappensBefore(left, right)

    def res(implicit state: SymbolicState): SVal[SortInvocationRes] =
      state.invocationRes.get(left)

    def op(implicit state: SymbolicState): SVal[SortInvocationInfo] =
      state.invocationOp.get(left)

    def calls(implicit state: SymbolicState): SVal[SortSet[SortCallId]] =
      state.invocationCalls.get(left)

  }

  implicit class OptionExtensions[T <: SymbolicSort](left: SVal[SortOption[T]]) {
    def isNone(implicit t: T) = (left === SNone(t))

  }

}


case class ConcreteVal[R, T <: SymbolicSort](value: R)(implicit val typ: T) extends SVal[T] {
}


case class SymbolicVariable[Sort <: SymbolicSort](
  name: String,
  isBound: Boolean,
  typ: Sort
) extends SVal[Sort] {
  override def toString: String = s"$name"
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

case class SChooseSome[T <: SymbolicSort](
  variable: SymbolicVariable[T],
  constraint: SVal[SortBoolean]) extends SVal[T] {
  override def typ: T = variable.typ
}

case class SReturnVal(methodName: UniqueName, value: SVal[SortValue]) extends SVal[SortInvocationRes] {
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

  override def typ: SortMap[K, V]

  def put(key: SVal[K], value: SVal[V]): SymbolicMap[K, V] = {
    SymbolicMapUpdated[K, V](
      updatedKey = key,
      newValue = value,
      baseMap = this
    )
  }


  def get(key: SVal[K]): SVal[V] =
    SMapGet(this, key)

}

case class SymbolicMapVar[K <: SymbolicSort, V <: SymbolicSort, KR](variable: SVal[SortMap[K, V]])
  (implicit val keySort: K, val valueSort: V)
  extends SymbolicMap[K, V] {

  override def typ: SortMap[K, V] = SortMap(keySort, valueSort)
}

object SymbolicMapVar {
  def symbolicMapVar[K <: SymbolicSort, V <: SymbolicSort](name: String)
    (implicit keySort: K, valueSort: V, ctxt: SymbolicContext): SymbolicMap[K, V] =
    SymbolicMapVar[K, V, Nothing](ctxt.makeVariable[SortMap[K, V]](name))
}


case class SymbolicMapEmpty[K <: SymbolicSort, V <: SymbolicSort](
  defaultValue: SVal[V]
)(implicit val keySort: K, valueSort: V) extends SymbolicMap[K, V] {
  override def typ: SortMap[K, V] = SortMap(keySort, valueSort)
}

// map updated with a symbolic key
case class SymbolicMapUpdated[K <: SymbolicSort, V <: SymbolicSort](
  updatedKey: SVal[K],
  newValue: SVal[V],
  baseMap: SVal[SortMap[K, V]]
) extends SymbolicMap[K, V] {

  override def put(key: SVal[K], value: SVal[V]): SymbolicMap[K, V] =
    if (key == updatedKey)
      this.copy(newValue = value)
    else
      super.put(key, value)

  override def typ: SortMap[K, V] = SortMap(updatedKey.typ, newValue.typ)
}


sealed abstract class SymbolicSet[T <: SymbolicSort] extends SVal[SortSet[T]] {
  def +(c: SVal[T]) =
    SSetInsert(this, Set(c))


  def isSubsetOf(other: SVal[SortSet[T]]): SVal[SortBoolean] = {
    IsSubsetOf(this, other)
  }

  def contains(value: SVal[T]): SVal[SortBoolean] = {
    SSetContains(this, value)
  }

  def union(other: SVal[SortSet[T]]): SymbolicSet[T] =
    SSetUnion(this, other)

  override def typ: SortSet[T]

}

case class SSetVar[T <: SymbolicSort](variable: SVal[SortSet[T]]) extends SymbolicSet[T] {
  override def typ: SortSet[T] = variable.typ
}

case class SSetEmpty[T <: SymbolicSort]()(implicit val t: T) extends SymbolicSet[T] {
  override def typ: SortSet[T] = SortSet(t)
}

case class SSetInsert[T <: SymbolicSort](set: SVal[SortSet[T]], values: Set[SVal[T]]) extends SymbolicSet[T] {

  override def +(c: SVal[T]) =
    this.copy(values = values + c)


  override def typ: SortSet[T] = set.typ
}


case class SSetUnion[T <: SymbolicSort](set1: SVal[SortSet[T]], set2: SVal[SortSet[T]]) extends SymbolicSet[T] {
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


case class SNamedVal[T <: SymbolicSort](name: String, value: SVal[T]) extends SVal[T] {
  override def typ: T = value.typ
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
  require(inType.constructors.contains(constructorName), s"$constructorName does not appear in $inType (constructors: ${inType.constructors.values.mkString(", ")})")


  override def typ: SortDatatype = dtyp
}

case class SCallInfo(operation: SVal[SortCustomDt]) extends SVal[SortCall] {
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

case class SValOpaque[T <: SymbolicSort](kind: Any, v: Any, typ: T) extends SVal[T] {
  override def toString: String =
  //    if (kind.toString == "UNINTERPRETED_CONSTANT")
    v.toString

  //    else
  //      super.toString
}
