package crdtver.symbolic

import crdtver.language.InputAst.InTypeExpr

import scala.language.existentials

/** a symbolic value
  *
  * the type parameter T is the sort of the value.
  * It is a phantom type (only used for type checking in the DSL)
  *
  * */
sealed abstract class SVal[T <: SymbolicSort] {

  def typ: T

  def ===(other: SVal[T]): SVal[SortBoolean] = SEq(this, other)

  def !==(other: SVal[T]): SVal[SortBoolean] = SNotEq(this, other)

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

  def datatype(typ: InTypeExpr, name: String, args: SVal[SortValue]*): SVal[SortValue] = SDatatypeValue(typ, name, args.toList)

  def datatype(typ: InTypeExpr, name: String, args: List[SVal[SortValue]]): SVal[SortValue] = SDatatypeValue(typ, name, args)

  implicit class MapGetExtension[K <: SymbolicSort, V <: SymbolicSort](mapExpr: SVal[SortMap[K, V]]) {
    def apply(key: SVal[K]): SMapGet[K, V] = SMapGet(mapExpr, key)
  }

}


case class ConcreteVal[R, T <: SymbolicSort](value: R)(implicit val typ: T) extends SVal[T] {
}


case class SymbolicVariable[Sort <: SymbolicSort](
  name: String,
  typ: Sort
) extends SVal[Sort]

case class SEq[T <: SymbolicSort](left: SVal[T], right: SVal[T]) extends SVal[SortBoolean] {
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


case class SNone[T <: SymbolicSort]()(implicit val ofTyp: T) extends SVal[SortOption[T]] {
  override def typ: SortOption[T] = SortOption(ofTyp)
}

case class SSome[T <: SymbolicSort](value: SVal[T]) extends SVal[SortOption[T]] {
  override def typ: SortOption[T] = SortOption(value.typ)
}

case class SReturnVal(methodName: String, value: SVal[SortValue]) extends SVal[SortInvocationRes] {
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
      case cv: ConcreteVal[KRep, K] @unchecked  =>
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
  def symbolicMapVar[K <: SymbolicSort, V <: SymbolicSort, KR](name: String)
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
      case cv: ConcreteVal[KRep, K] @unchecked  =>
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
      case cv: ConcreteVal[KRep, K] @unchecked  =>
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
    for ((k,v) <- currentKnowledge) {
      res = SymbolicMapUpdated(concrete.makeValue(k), v, res)
    }
    res
  }

}


sealed abstract class SymbolicSet[T <: SymbolicSort] extends SVal[SortSet[T]] {

  def isSubsetOf(other: SVal[SortSet[T]]): SVal[SortBoolean] = {
    IsSubsetOf(this, other)
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

case class SSetContains[T <: SymbolicSort](set: SymbolicSet[T], value: SVal[T]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}


sealed abstract class Quantifier

case class QForall() extends Quantifier

case class QExists() extends Quantifier

case class QuantifierExpr(
  quantifier: Quantifier,
  variable: SymbolicVariable[_ <: SymbolicSort],
  body: SVal[SortBoolean]
) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class Committed() extends SVal[SortTransactionStatus] {
  override def typ: SortTransactionStatus = SortTransactionStatus()
}

case class Uncommitted() extends SVal[SortTransactionStatus] {
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


case class SDatatypeValue(inType: InTypeExpr, constructorName: String, values: List[SVal[SortValue]]) extends SVal[SortValue] {
  override def typ: SortValue = SortValue(inType)
}


case class SInvocationInfo(procname: String, args: List[SVal[SortValue]]) extends SVal[SortInvocationInfo] {
  override def typ: SortInvocationInfo = SortInvocationInfo()
}

case class MapDomain[K <: SymbolicSort, V <: SymbolicSort](map: SVal[SortMap[K, SortOption[V]]]) extends SVal[SortSet[K]] {
  override def typ: SortSet[K] = SortSet(map.typ.keySort)
}

case class IsSubsetOf[T <: SymbolicSort](left: SVal[SortSet[T]], right: SVal[SortSet[T]]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}