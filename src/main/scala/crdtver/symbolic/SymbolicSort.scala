package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.{BoolType, InTypeExpr, IntType}

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

  implicit def uid: SortUid = SortUid()

  implicit def invocationInfo: SortInvocationInfo = SortInvocationInfo()

//  implicit def value: SortValue = SortValue()

  implicit def invocationRes: SortInvocationRes = SortInvocationRes()

  implicit def option[T <: SymbolicSort](implicit s: T): SortOption[T] = SortOption(s)

  implicit def set[T <: SymbolicSort](implicit s: T): SortSet[T] = SortSet(s)

}

/** a type class for sorts that can be represented by concrete values */
sealed abstract class SymbolicSortConcrete[T <: SymbolicSort, R] {
  def makeValue(r: R): ConcreteVal[R, T]
  def getValue(cv: ConcreteVal[R, T]): R
}

object SymbolicSortConcrete {
  implicit def bool(): SymbolicSortConcrete[SortBoolean, Boolean] = default
  implicit def invocationId(): SymbolicSortConcrete[SortInvocationId, Nothing] = default

  def default[T <: SymbolicSort, R](implicit t: T): SymbolicSortConcrete[T, R] =
      new SymbolicSortConcrete[T, R] {
        override def makeValue(r: R): ConcreteVal[R, T] =
          ConcreteVal(r)

        override def getValue(cv: ConcreteVal[R, T]): R =
          cv.value
      }
}



// type for values usable in programs
sealed abstract class SortValue extends SymbolicSort


case class SortInt() extends SortValue() {

}

case class SortBoolean() extends SortValue() {

}

// for user defined types in Repliss (id types and algebraic data types)
case class SortCustom(typ: SortDatatypeImpl) extends SortValue()

//case class SymbolicStateSort() extends SymbolicSort


case class SortCallId() extends SymbolicSort

case class SortTxId() extends SymbolicSort

case class SortTransactionStatus() extends SymbolicSort

case class SortInvocationId() extends SymbolicSort

case class SortCall() extends SymbolicSort

case class SortUid() extends SymbolicSort

// includes datatype value
case class SortInvocationInfo() extends SortDatatype

case class SortInvocationRes() extends SortDatatype

case class SortMap[K <: SymbolicSort, V <: SymbolicSort](
  keySort: K, valueSort: V
) extends SymbolicSort

case class SortSet[T <: SymbolicSort](
  valueSort: T
) extends SymbolicSort


case class SortOption[+T <: SymbolicSort](
  valueSort: T
) extends SymbolicSort

sealed abstract class SortDatatype extends SymbolicSort {
}

case class SortDatatypeImpl(
  name: String,
  constructors: Map[String, DatatypeConstructor]
)

case class DatatypeConstructor(
  name: String,
  args: List[SymbolicVariable[_ <: SymbolicSort]]
)