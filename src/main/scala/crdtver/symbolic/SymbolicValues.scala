package crdtver.symbolic

import crdtver.language.InputAst.InTypeExpr

/** a symbolic value
  *
  * the type parameter T is the sort of the value.
  * It is a phantom type (only used for type checking in the DSL)
  *
  * */
sealed abstract class SVal[T <: SymbolicSort] {
  def ===(other: SVal[T]): SVal[SortBoolean] = SEq(this, other)
}


case class ConcreteVal[T <: SymbolicSort](
  value: T
) extends SVal[T]


case class SymbolicVariable[Sort <: SymbolicSort](
  name: String,
  typ: Sort
) extends SVal[Sort]

case class SEq[T <: SymbolicSort](left: SVal[T], right: SVal[T]) extends SVal[SortBoolean]

case class SNone[T <: SymbolicSort]() extends SVal[SortOption[T]]

case class SSome[T <: SymbolicSort](value: SVal[T]) extends SVal[SortOption[T]]

// TODO could make application more generic and use Hlists (whooo!)
// case class SApp(func: SFunc, args: List[SVal[_]])

case class SMapGet[K <: SymbolicSort, V <: SymbolicSort](map: SVal[SortMap[K, V]], key: SVal[K]) extends SVal[V] {


}

sealed abstract class SymbolicMap[K <: SymbolicSort, V <: SymbolicSort] extends SVal[SortMap[K, V]] {
  def put(key: SVal[K], value: SVal[V]): SymbolicMap[K, V] = {
    key match {
      case ConcreteVal(k) =>
        SymbolicMapUpdatedConcrete(
          currentKnowledge = Map(k -> value),
          baseMap = this
        )
      case _ => SymbolicMapUpdated(
        updatedKey = key,
        newValue = value,
        baseMap = this
      )
    }
  }

  def get(key: SVal[K]): SVal[V] =
    SMapGet(this, key)
}

case class SymbolicMapEmpty[K <: SymbolicSort, V <: SymbolicSort](
  defaultValue: SVal[V]
) extends SymbolicMap[K, V]

// map updated with a symbolic key
case class SymbolicMapUpdated[K <: SymbolicSort, V <: SymbolicSort](
  updatedKey: SVal[K],
  newValue: SVal[V],
  baseMap: SVal[SortMap[K, V]]
) extends SymbolicMap[K, V] {
}

// map updated with concrete keys
// (optimization suggested by Symbooglix paper)
case class SymbolicMapUpdatedConcrete[K <: SymbolicSort, V <: SymbolicSort](
  currentKnowledge: Map[K, SVal[V]],
  baseMap: SVal[SortMap[K, V]]
) extends SymbolicMap[K, V] {
  override def put(key: SVal[K], value: SVal[V]): SymbolicMap[K, V] = {
    key match {
      case ConcreteVal(k) =>
        copy(
          currentKnowledge = currentKnowledge + (k -> value)
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
      case ConcreteVal(k) if currentKnowledge.contains(k) =>
        currentKnowledge(k)
      case _ =>
        super.get(key)
    }
  }
}


sealed abstract class SymbolicSet[T <: SymbolicSort] extends SVal[SortSet[T]]

case class SSetEmpty[T <: SymbolicSort]() extends SymbolicSet[T]

case class SSetInsert[T <: SymbolicSort](set: SymbolicSet[T], value: T) extends SymbolicSet[T]

case class SSetContains[T <: SymbolicSort](set: SymbolicSet[T], value: T) extends SVal[SortBoolean]


