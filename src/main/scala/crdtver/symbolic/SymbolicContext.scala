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
  private var indent: Int = 0
  private val debug = true
//  private val programTypes: Map[String, SymbolicSort] = initProgramTypes(z3Translation.prog)
//
//
//  private def initProgramTypes(prog: InProgram): Map[String, SymbolicSort] = {
//    // should I do translation to z3 here as well?
//    // TODO go through program and create special sorts
//    // or just use generic sorts and only create them for z3?
//    ???
//  }



  private def printIndent(): String = "  ".repeat(indent)

  private def debugPrint(s: => String): Unit = {
    if (debug) {
      val lines = s.lines()
      lines.forEach(line =>
        println(printIndent() + line))
    }
  }

  def addConstraint(constraint: SVal[SortBoolean]): Unit = {
    val translated = z3Translation.translateBool(constraint)(z3Translation.freshContext())
    debugPrint(s"addConstraint $constraint")
    indent += 2
    debugPrint(s"$translated")
    indent -= 2
    solver.add(translated)
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
  def translateSort(typ: InputAst.InTypeExpr): SymbolicSort =
    ExprTranslation.translateType(typ)(this)

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
    debugPrint(s"push")
    indent += 1
    val r = branch()
    debugPrint(s"pop")
    indent -= 1
    solver.pop()
    r
  }


  def check(): SolverResult = {
    val checkRes = solver.check()
    debugPrint("check: " + checkRes)
    checkRes match {
      case Status.UNSATISFIABLE => Unsatisfiable
      case Status.UNKNOWN => Unknown
      case Status.SATISFIABLE => SatisfiableH
    }
  }

  def getModel(proofThatItIsSatisfiable: Satisfiable): SModel = {
    new SModel(solver.getModel, z3Translation)
  }

}

class SModel(model: Model, z3Translation: Z3Translation) {


  def executeForExpr[T <: SymbolicSort](x: SVal[T]): Expr = {
    model.eval(z3Translation.translateExpr(x)(z3Translation.freshContext()), true)
  }

  def executeForString[T <: SymbolicSort](x: SVal[T]): String = {
    executeForExpr(x).toString
  }

}

object SymbolicContext {

  sealed abstract class SolverResult

  case object Unsatisfiable extends SolverResult

  case object Unknown extends SolverResult

  abstract class Satisfiable extends SolverResult

  private case object SatisfiableH extends Satisfiable

}



