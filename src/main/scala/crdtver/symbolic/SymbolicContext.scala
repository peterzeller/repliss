package crdtver.symbolic

import com.microsoft.z3._
import crdtver.language.InputAst
import crdtver.language.InputAst._
import crdtver.symbolic.SymbolicContext._

class SymbolicContext(
  z3Translation: Z3Translation,
  val currentProcedure: String
) {


  private val context: Context = z3Translation.ctxt
  private val solver: Solver = context.mkSolver()
  private var usedVariables: Set[String] = Set()

  def addConstraint(constraint: SVal[SortBoolean]): Unit = {
    solver.add(z3Translation.translateBool(constraint)(z3Translation.freshContext()))
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

  def translateExpr[T <: SymbolicSort](expr: InExpr)(implicit sort: T, state: SymbolicState): SVal[T] =
    ExprTranslation.translate(expr)(sort, this, state)

  def translateExprV(expr: InExpr)(implicit state: SymbolicState): SVal[SortValue] =
      ExprTranslation.translate(expr)(SortValue(expr.getTyp), this, state)

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



  def check(): SolverResult = {
    solver.check() match {
      case Status.UNSATISFIABLE => Unsatisfiable
      case Status.UNKNOWN => Unknown
      case Status.SATISFIABLE => SatisfiableH
    }
  }

  def getModel(proofThatItIsSatisfiable: Satisfiable): AnyRef = {
    val model = solver.getModel
    model
  }

}

object SymbolicContext {

  sealed abstract class SolverResult

  case object Unsatisfiable extends SolverResult

  case object Unknown extends SolverResult

  abstract class Satisfiable extends SolverResult

  private case object SatisfiableH extends Satisfiable

}



