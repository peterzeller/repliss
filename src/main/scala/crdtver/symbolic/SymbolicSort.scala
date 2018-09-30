package crdtver.symbolic

/** a symbolic Type/Sort */
sealed abstract class SymbolicSort {
  def ::(name: String): SymbolicVariable[this.type] = SymbolicVariable(name, this)
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

  implicit def uid: SortUid = SortUid()

  implicit def invocationInfo: SortInvocationInfo = SortInvocationInfo()

  implicit def value: SortValue = SortValue()

  implicit def option[T <: SymbolicSort](implicit s: T): SortOption[T] = SortOption(s)

  implicit def set[T <: SymbolicSort](implicit s: T): SortSet[T] = SortSet(s)

}

case class SortInt() extends SymbolicSort

case class SortBoolean() extends SymbolicSort

case class SymbolicStateSort() extends SymbolicSort


case class SortCallId() extends SymbolicSort

case class SortTxId() extends SymbolicSort

case class SortTransactionStatus() extends SymbolicSort

case class SortInvocationId() extends SymbolicSort

case class SortCall() extends SymbolicSort

case class SortUid() extends SymbolicSort

// includes datatype value
case class SortInvocationInfo() extends SymbolicSort

case class SortValue() extends SymbolicSort

case class SortMap[K <: SymbolicSort, V <: SymbolicSort](
  keySort: K, valueSort: V
) extends SymbolicSort

case class SortSet[T <: SymbolicSort](
  valueSort: T
) extends SymbolicSort


case class SortOption[+T <: SymbolicSort](
  valueSort: T
) extends SymbolicSort