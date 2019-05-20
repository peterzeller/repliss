package crdtver.symbolic

import crdtver.language.crdts.UniqueName

/** a symbolic Type/Sort */
sealed abstract class SymbolicSort {
  //  def ::[T >: this.type <: SymbolicSort](name: String): SymbolicVariable[T] = SymbolicVariable(name, this.asInstanceOf[T])
}

/** implicit definitions for making sorts */
object SymbolicSort {

  implicit def sortMap[K <: SymbolicSort, V <: SymbolicSort](implicit keySort: K, valueSort: V): SortMap[K, V] = SortMap(keySort, valueSort)

  implicit def callId: SortCallId = SortCallId()

  implicit def int: SortInt = SortInt()

  implicit def bool: SortBoolean = SortBoolean()

  implicit def call: SortCall = SortCall()

  implicit def invocationId: SortInvocationId = SortInvocationId()

  implicit def txId: SortTxId = SortTxId()

  implicit def transactionStatus: SortTransactionStatus = SortTransactionStatus()

  implicit def invocationInfo: SortInvocationInfo = SortInvocationInfo()

  //  implicit def value: SortValue = SortValue()

  implicit def invocationRes: SortInvocationRes = SortInvocationRes()

  implicit def option[T <: SymbolicSort](implicit s: T): SortOption[T] = SortOption(s)

  implicit def set[T <: SymbolicSort](implicit s: T): SortSet[T] = SortSet(s)

}


// type for values usable in programs
sealed abstract class SortValue extends SymbolicSort


case class SortInt() extends SortValue() {

}

case class SortBoolean() extends SortValue() {

}

/** a special sort in case the sort is not known */
case class SortAny() extends SymbolicSort()

// for user defined types in Repliss (id types and algebraic data types)
case class SortCustomDt(typ: SortDatatypeImpl) extends SortValue() with SortDatatype {
  override def toString: String = typ.toString
}

case class SortCustomUninterpreted(name: String) extends SortValue()

//case class SymbolicStateSort() extends SymbolicSort


case class SortCallId() extends SymbolicSort

case class SortTxId() extends SymbolicSort

case class SortTransactionStatus() extends SymbolicSort with SortDatatype

case class SortInvocationId() extends SymbolicSort

case class SortCall() extends SortDatatype

// includes datatype value
case class SortInvocationInfo() extends SortDatatype

case class SortInvocationRes() extends SortDatatype

final case class SortMap[K <: SymbolicSort, V <: SymbolicSort](
  keySort: K, valueSort: V
) extends SymbolicSort

case class SortSet[T <: SymbolicSort](
  valueSort: T
) extends SymbolicSort


case class SortOption[+T <: SymbolicSort](
  valueSort: T
) extends SymbolicSort

sealed trait SortDatatype extends SymbolicSort {
}

case class SortDatatypeImpl(
  name: String,
  constructors: Map[String, DatatypeConstructor]
) {
  require(constructors.nonEmpty, "There must be at least one constructor.")

  override def toString: String =
    name

  //s"(type $name = ${constructors.values.mkString(" | ")}"
}

case class DatatypeConstructor(
  name: String,
  args: List[SymbolicVariable[_ <: SymbolicSort]]
) {
  override def toString: String = s"$name(${args.mkString(", ")})"
}