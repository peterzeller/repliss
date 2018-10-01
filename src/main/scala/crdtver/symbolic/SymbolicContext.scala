package crdtver.symbolic

import com.microsoft.z3._
import crdtver.language.InputAst
import crdtver.language.InputAst._
import crdtver.symbolic.SymbolicContext.{Satisfiable, SolverResult, Unknown, Unsatisfiable}

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

  def translateSortVal(typ: InputAst.InTypeExpr): SortValue = {
    translateSort(typ).asInstanceOf[SortValue]
  }



  /**
    * Executes some code in a new context.
    *
    * When executing it on Z3 this means
    * pushing a new frame when entering the code block
    * and popping a frame afterwards
    **/
  def inContext[T](branch: () => T): T = {
    solver.push()
    val r = branch()
    solver.pop()
    r
  }


  def addUniqueConstant[T <: SymbolicSort](name: String, sort: T): SymbolicVariable[T] = ???

  def check(): SolverResult = {
    solver.check() match {
      case Status.UNSATISFIABLE => Unsatisfiable()
      case Status.UNKNOWN => Unknown()
      case Status.SATISFIABLE => new Satisfiable()
    }
  }

  def getModel(proofThatItIsSatisfiable: Satisfiable): AnyRef = {
    val model = solver.getModel
    model
  }

}

object SymbolicContext {

  sealed abstract class SolverResult

  case class Unsatisfiable() extends SolverResult

  case class Unknown() extends SolverResult

  class Satisfiable private[SymbolicContext]() extends SolverResult

}



