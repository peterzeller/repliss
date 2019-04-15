package crdtver.symbolic

import java.util.concurrent.TimeUnit

import com.microsoft.z3._
import crdtver.language.TypedAst
import crdtver.language.TypedAst._
import crdtver.language.crdts.CrdtTypeDefinition
import crdtver.language.crdts.CrdtTypeDefinition.Param
import crdtver.symbolic.SymbolicContext.{Unsatisfiable, _}
import crdtver.symbolic.smt.Cvc4Solver
import crdtver.utils.ListExtensions._
import edu.nyu.acsys.CVC4
import scalaz.Memo

import scala.collection.immutable.WrappedString
import scala.concurrent.duration.Duration

case class NamedConstraint(description: String, constraint: SVal[SortBoolean], translated: CVC4.Expr)

class SymbolicContext(
  val z3Translation: SmtTranslation,
  val currentProcedure: String,
  prog: InProgram
) {

  private val solver: smt.Solver = new Cvc4Solver()
  private var usedVariables: Set[String] = Set()
  private var indent: Int = 0
  private val debug = false
  private var constraints: List[List[NamedConstraint]] = List(List())

  private val simplifier: Simplifier = new Simplifier(this)


  //  private val programTypes: Map[String, SymbolicSort] = initProgramTypes(z3Translation.prog)
  //
  //
  //  private def initProgramTypes(prog: InProgram): Map[String, SymbolicSort] = {
  //    // should I do translation to z3 here as well?
  //    // TODO go through program and create special sorts
  //    // or just use generic sorts and only create them for z3?
  //    ???
  //  }


  def allConstraints(): List[NamedConstraint] =
    constraints.flatten.reverse

  def allConstraintsSimplified(): List[NamedConstraint] =
      for (c <- constraints.flatten.reverse) yield
        c.copy(constraint = simplifier.simp(c.constraint))

  private def printIndent(): String = {
    val s = new StringBuilder()
    for (i <- 0 to indent)
      s.append("  ")
    s.toString()
  }

  private def debugPrint(s: => String): Unit = {
    if (debug) {
      for (line <- new WrappedString(s).lines)
        println(printIndent() + line)
    }
  }

  def findQuery(name: String): Option[InQueryDecl] =
    prog.programCrdt.queryDefinitions()
      .find(_.name.name == name)


  def addConstraint(what: String, constraint: SVal[SortBoolean]): Unit = {
    constraint match {
      case SAnd(l, r) =>
        addConstraint(what + "l", l)
        addConstraint(what + "r", r)
      case _ =>


        debugPrint(s"addConstraint ${constraint.toString.replace("\n", "\n               ")}")
        val sContstraint = simplifier.simp(constraint)
        val translated = z3Translation.translateBool(sContstraint)
        constraints = (NamedConstraint(what, constraint, translated.asInstanceOf[CVC4.Expr]) :: constraints.head) :: constraints.tail
        indent += 2
        debugPrint(s"$translated")
        indent -= 2
        try {
          solver.add(translated)
        } catch {
          case e: Exception =>
            throw new RuntimeException(s"Could not add constraint $what: $constraint", e)
        }
    }
  }

  def makeBoundVariable[T <: SymbolicSort](name: String)(implicit sort: T): SymbolicVariable[T] = {
    makeVariable(s"bound_$name")(sort)
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
  val translateSort: TypedAst.InTypeExpr => SymbolicSort = Memo.mutableHashMapMemo { typ =>
    ExprTranslation.translateType(typ)(this)
  }

  def translateSortVal(typ: TypedAst.InTypeExpr): SortValue = {
    translateSort(typ).asInstanceOf[SortValue]
  }

  def translateSortCustomUninterpreted(typ: TypedAst.InTypeExpr): SortCustomUninterpreted = {
    translateSort(typ).asInstanceOf[SortCustomUninterpreted]
  }


  def translateSortDatatype(typ: TypedAst.InTypeExpr): SortDatatype = {
    translateSort(typ).asInstanceOf[SortDatatype]
  }

  def translateSortDatatypeToImpl(typ: TypedAst.InTypeExpr): SortDatatypeImpl = {
    datypeImpl(translateSort(typ).asInstanceOf[SortDatatype])
  }

  def translateExpr[T <: SymbolicSort](expr: InExpr)(implicit sort: T, state: SymbolicState): SVal[T] =
    ExprTranslation.translate(expr)(sort, this, state)

  def translateExprV(expr: InExpr)(implicit state: SymbolicState): SVal[SortValue] =
    ExprTranslation.translate(expr)(translateSortVal(expr.getTyp), this, state)

  /**
    * Executes some code in a new context.
    *
    * When executing it on Z3 this means
    * pushing a new frame when entering the code block
    * and popping a frame afterwards
    **/
  def inContext[T](branch: () => T): T = {
    solver.push()
    constraints = List() :: constraints
    debugPrint(s"push")
    indent += 1
    val r = branch()
    debugPrint(s"pop")
    indent -= 1
    constraints = constraints.tail
    solver.pop()
    r
  }


  def check(): SolverResult = {
    val checkRes = solver.check()
    debugPrint("check: " + checkRes)
    checkRes match {
      case solver.Unsatisfiable() => SymbolicContext.Unsatisfiable
      case solver.Unknown() => SymbolicContext.Unknown
      case s: solver.Satisfiable => Satisfiable(new SymbolicContext.Model {
        val m = s.getModel

        /** evaluates a symbolic value to a string representation */
        override def evaluate[T <: SymbolicSort](expr: SVal[T]): SVal[T] = {
          val sExpr = simplifier.simp(expr)
          val r: z3Translation.TExpr = m.eval(z3Translation.translateExpr(sExpr), true)
          z3Translation.parseExpr(r)(expr.typ)
        }

        override def toString: String =
          m.toString
      })


    }
  }


  val datypeImpl: SortDatatype => SortDatatypeImpl = Memo.mutableHashMapMemo {
    case SortInvocationInfo() =>
      invocationInfoType
    case SortInvocationRes() =>
      returnDatatype
    case SortCall() =>
      callType
    case SortTransactionStatus() =>
      transactionStatusType
    case SortCustomDt(impl) =>
      impl
  }


  val getIdType: IdType => SortDatatypeImpl = Memo.mutableHashMapMemo { idT: IdType =>
    SortDatatypeImpl(idT.name, Map())
  }

  val getCustomType: SimpleType => SortValue = Memo.mutableHashMapMemo { t: SimpleType =>
    val decl: InTypeDecl = prog.types.find(_.name.name == t.name).getOrElse(throw new RuntimeException(s"Could not find type $t"))
    if (decl.dataTypeCases.isEmpty) {
      SortCustomUninterpreted(decl.name.name)
    } else {
      val constructors: Map[String, DatatypeConstructor] =
        for (c <- decl.dataTypeCases) yield
          c.name.name -> DatatypeConstructor(
            c.name.name,
            c.params.map((variable: InVariable) => makeVariable(variable.name.name)(translateSort(variable.typ))))
      SortCustomDt(SortDatatypeImpl(t.name, constructors))
    }
  }

  private lazy val invocationInfoType: SortDatatypeImpl = {
    val constructors: Map[String, DatatypeConstructor] = prog.procedures.map { proc =>
      val name = proc.name.name
      val args: List[SymbolicVariable[_ <: SymbolicSort]] =
        proc.params.map(p => makeVariable(p.name.name)(translateSort(p.typ)))
      val constr = DatatypeConstructor(name, args)

      name -> constr
    }
    val noInvoc = DatatypeConstructor("no_invocation", List())

    SortDatatypeImpl("invocationInfo", constructors + (noInvoc.name -> noInvoc))
  }

  private lazy val callType: SortDatatypeImpl = {

    val constructorsOps: Map[String, DatatypeConstructor] = prog.programCrdt.operations().map { operation: CrdtTypeDefinition.Operation =>
      val name: String = operation.name
      var i = 0
      val args: List[SymbolicVariable[_ <: SymbolicSort]] =
        operation.params.map(p => makeVariable(p.name)(translateSort(p.typ)))
      val constr = DatatypeConstructor(name, args)

      name -> constr
    }
    val constructorsQueries: Map[String, DatatypeConstructor] = prog.programCrdt.queries().map { query: CrdtTypeDefinition.Query =>
      val name: String = s"queryop_${query.qname}"
      var i = 0
      val args: List[SymbolicVariable[_ <: SymbolicSort]] =
        query.params.map(p => makeVariable(p.name)(translateSort(p.typ)))
      val args2: List[SymbolicVariable[_ <: SymbolicSort]] = args :+ makeVariable("result")(translateSort(query.qreturnType))
      val constr = DatatypeConstructor(name, args2)

      name -> constr
    }
    val constructors = constructorsOps ++ constructorsQueries

    val noInvoc = DatatypeConstructor("no_call", List())

    SortDatatypeImpl("callInfo", constructors + (noInvoc.name -> noInvoc))
  }

  private lazy val transactionStatusType: SortDatatypeImpl = {

    val constructors: Map[String, DatatypeConstructor] = Map(
      "Uncommitted" -> DatatypeConstructor("Uncommitted", List()),
      "Committed" -> DatatypeConstructor("Committed", List())
    )

    SortDatatypeImpl("transactionStatus", constructors)
  }

  private lazy val returnDatatype: SortDatatypeImpl = {
    val constructors: Map[String, DatatypeConstructor] = prog.procedures.map { proc =>
      val name = s"${proc.name.name}_res"
      val args: List[SymbolicVariable[_ <: SymbolicSort]] =
        proc.returnType match {
          case Some(rt) =>
            List(makeVariable(name + "_arg")(translateSort(rt)))
          case None =>
            List()
        }

      val constr = DatatypeConstructor(name, args)
      name -> constr
    }
    val noInvoc = DatatypeConstructor("NoResult", List())

    SortDatatypeImpl("invocationResult", constructors + (noInvoc.name -> noInvoc))
  }


}

object SymbolicContext {

  sealed abstract class SolverResult

  case object Unsatisfiable extends SolverResult

  case object Unknown extends SolverResult

  case class Satisfiable(model: Model) extends SolverResult

  trait Model {
    /** evaluates a symbolic value to a string representation */
    def evaluate[T <: SymbolicSort](expr: SVal[T]): SVal[T]

  }


}



