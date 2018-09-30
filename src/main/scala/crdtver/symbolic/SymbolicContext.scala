package crdtver.symbolic

import com.microsoft.z3._
import crdtver.language.InputAst
import crdtver.language.InputAst._

class SymbolicContext {
  private val context = new Context()
  private val solver = context.mkSolver()
  private var usedVariables: Set[String] = Set()

  def addConstraint(constraint: SVal[SortBoolean]): Unit = {
    solver.add(Z3Translation.translateBool(constraint)(context))
  }

  def makeVariable[T <: SymbolicSort](name: String)(implicit sort: T): SymbolicVariable[T] = {
    var n = name
    var i = 0
    while (usedVariables contains n) {
      i += 1
      n = name + i
    }
    usedVariables += n
    SymbolicVariable(n, sort)
  }


  /**
    * translate a sort from InTypeExpr to SymbolicSort
    *
    * Creates a new sort if necessary
    **/
  def translateSort(typ: InputAst.InTypeExpr): SymbolicSort = typ match {
    case CallIdType() => SortCallId()
    case BoolType() => SortBoolean()
    case IntType() => SortInt()
    case InvocationResultType() => ???
    case FunctionType(argTypes, returnType, source) => ???
    case UnresolvedType(name, source) => ???
    case TransactionIdType() => ???
    case InvocationInfoType() => ???
    case AnyType() => ???
    case InvocationIdType() => ???
    case SomeOperationType() => ???
    case IdType(name, source) => ???
    case UnknownType() => ???
    case SimpleType(name, source) => ???
    case OperationType(name, source) => ???

  }



  /**
    * Executes some code in a new context.
    *
    * When executing it on Z3 this means
    * pushing a new frame when entering the code block
    * and popping a frame afterwards
    **/
  def inContext(branch: () => Unit): Unit = {
    solver.push()
    branch()
    solver.pop()
  }


  def addUniqueConstant[T <: SymbolicSort](name: String, sort: T): SymbolicVariable[T] = ???

}


