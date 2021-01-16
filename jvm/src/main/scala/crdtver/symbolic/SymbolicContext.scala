package crdtver.symbolic

import java.time.Duration
import java.util.concurrent.TimeUnit

import crdtver.RunArgs
import crdtver.language.TypedAst
import crdtver.language.TypedAst._
import crdtver.symbolic.ExprTranslation.translateType
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.smt._
import crdtver.utils.ListExtensions._
import crdtver.utils.{ConcurrencyUtils, DurationUtils, MathUtils, myMemo}
import codes.reactive.scalatime._
import crdtver.utils.DurationUtils.ScalaDurationExt

import scala.math

case class NamedConstraint(description: String, priority: Int, constraint: SVal[SortBoolean])

class SymbolicContext(
  val smtTranslation: ToSmtTranslation,
  val currentProcedure: String,
  val prog: InProgram,
  runArgs: RunArgs
) {

  private var usedVariables: Set[String] = Set()
  private var indent: Int = 0
  private val debug = false

  private val simplifier: Simplifier = new Simplifier(this)


  private def printIndent(): String = {
    val s = new StringBuilder()
    for (i <- 0 to indent)
      s.append("  ")
    s.toString()
  }

  private def debugPrint(s: => String): Unit = {
    if (debug) {
      for (line <- s.linesIterator)
        println(printIndent() + line)
    }
  }

  def findQuery(name: String): Option[InQueryDecl] =
    prog.programCrdt.queryDefinitions()
      .find(_.name.name == name)


  def makeBoundVariable[T <: SymbolicSort](name: String)(implicit sort: T): SymbolicVariable[T] = {
    makeVariable(s"bound_$name", true)(sort)
  }

  def makeVariable[T <: SymbolicSort](name: String, bound: Boolean = false)(implicit sort: T): SymbolicVariable[T] = {
    var n = name
    var i = 0
    while (usedVariables contains n) {
      i += 1
      n = name + i
    }
    usedVariables += n
    SymbolicVariable(n, bound, sort)
  }


  /**
   * translate a sort from InTypeExpr to SymbolicSort
   *
   * Creates a new sort if necessary
   **/
  val translateSort: TypedAst.InTypeExpr => SymbolicSort = new myMemo({ typ =>
    ExprTranslation.translateType(typ)(this)
  })

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

  private def translateConstraints(constraints: List[NamedConstraint]): List[Smt.NamedConstraint] =
    for (nc <- constraints) yield
      Smt.NamedConstraint(nc.description, nc.priority, smtTranslation.translateBool(nc.constraint))



  private val uniqueIds_funcM: myMemo[(SymbolicSort, IdType), UninterpretedFunction[SortSet[SortCustomUninterpreted]]] = new myMemo[(SymbolicSort, IdType), UninterpretedFunction[SortSet[SortCustomUninterpreted]]]({e =>
    val typ = e._1
    val t = e._2
    val returnType = SortSet(translateType(t))
    val typName = typ.toString
      .replaceAll("[^a-zA-Z]+", "_")
      .replaceAll("_+$", "")
    UninterpretedFunction(s"uniqueIds_op_${t.name}_$typName", List(typ), returnType)
  })

  def uniqueIds_func(t: SymbolicSort, idType: IdType): UninterpretedFunction[SortSet[SortCustomUninterpreted]] =
    uniqueIds_funcM((t, idType))

  def uniqueIdFuncs: Iterable[((SymbolicSort, IdType), UninterpretedFunction[SortSet[SortCustomUninterpreted]])] = uniqueIds_funcM

  /**
   * Checks a list of constraints for satisfiability.
   *
   */
  def check(constraints: List[NamedConstraint], baseName: String, explainResult: Boolean, maxTimeLimit: Duration = DurationUtils.maxDuration): SolverResult = {
    val rLimit = ResourceLimit(1000000)
    val tLimit = SmtTimeout(MathUtils.min(maxTimeLimit, runArgs.timeout.toJava))
    val limits = List(rLimit, tLimit)

    val sharedOptions = limits ++
      (if (explainResult) List(SmtBuildUnsatCore(), SmtBuildModel()) else List(SmtBuildModel()))


    val translatedConstraints: List[Smt.NamedConstraint] = translateConstraints(constraints)

    val solver = runArgs.solver

    checkWithOptions(constraints, translatedConstraints, CheckOptions(solver, sharedOptions), baseName)
  }

  case class CheckOptions(solver: Solver, extraOptions: List[SmtOption])


  private def checkWithOptions(contraints: List[NamedConstraint], translatedConstraints: List[Smt.NamedConstraint], options: CheckOptions, name: String): SolverResult = {
    val solver: smt.Solver = options.solver
    val checkRes = solver.check(translatedConstraints, options.extraOptions, name)
    debugPrint("check: " + checkRes)
    checkRes match {
      case Solver.Unsatisfiable(unsatCore) =>
        val unsatCoreOrig: List[NamedConstraint] = contraints.filter(c => unsatCore.exists(c2 => c.description == c2.description))
        SymbolicContext.Unsatisfiable(unsatCoreOrig)
      case Solver.Unknown() =>
        SymbolicContext.Unknown
      case s: Solver.Satisfiable =>
        Satisfiable(s.isIncomplete, new Model {
          val m: Solver.Model = s.getModel

          /** evaluates a symbolic value to a string representation */
          override def evaluate[T <: SymbolicSort](expr: SVal[T]): SVal[T] = {
            val sExpr = simplifier.simp(expr)
            val r: Smt.SmtExpr = m.eval(smtTranslation.translateExpr(sExpr), true)
            smtTranslation.parseExpr(r, expr.typ)
          }

          override def toString: String =
            m.toString
        })
    }
  }

  def exportConstraints(constraints: List[NamedConstraint]): String = {
    val smtConstraints: List[Smt.NamedConstraint] = for (c <- constraints) yield {
      Smt.NamedConstraint(c.description, c.priority, smtTranslation.translateExpr(c.constraint))
    }
    SmtLibPrinter.print(smtConstraints).prettyStr(120)
  }

  def exportConstraintsToSmt(constraints: List[NamedConstraint]): String = {
    val smtConstraints: List[Smt.NamedConstraint] = for (c <- constraints) yield
      Smt.NamedConstraint(c.description, c.priority, smtTranslation.translateExpr(c.constraint))
    SmtLibPrinter.print(smtConstraints).prettyStr(120)
  }


  val datypeImpl: SortDatatype => SortDatatypeImpl = new myMemo({
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
  })


  val getIdType: IdType => SortDatatypeImpl = new myMemo({ idT: IdType =>
    SortDatatypeImpl(idT.name, Map())
  })

  val getCustomType: SimpleType => SortValue = new myMemo({ t: SimpleType =>
    val decl: InTypeDecl = prog.types.find(_.name.name == t.name).getOrElse(throw new RuntimeException(s"Could not find type $t"))
    if (decl.dataTypeCases.isEmpty) {
      SortCustomUninterpreted(decl.name.name)
    } else {
      val constructors: Map[String, DatatypeConstructor] =
        (for (c <- decl.dataTypeCases) yield
          c.name.name -> DatatypeConstructor(
            c.name.name,
            c.params.map((variable: InVariable) => makeVariable(variable.name.name)(translateSort(variable.typ)))))
          .toMap
      SortCustomDt(SortDatatypeImpl(t.name, constructors))
    }
  })

  private lazy val invocationInfoType: SortDatatypeImpl = {
    val constructors: Map[String, DatatypeConstructor] = prog.procedures.map { proc =>
      val name = proc.name.name
      val args: List[SymbolicVariable[_ <: SymbolicSort]] =
        proc.params.map(p => makeVariable(p.name.name)(translateSort(p.typ)))
      val constr = DatatypeConstructor(name, args)

      name -> constr
    }
      .toMap
    val noInvoc = DatatypeConstructor("no_invocation", List())

    SortDatatypeImpl("invocationInfo", constructors + (noInvoc.name -> noInvoc))
  }

  private lazy val callType: SortDatatypeImpl = {

    val constructors = List(
      DatatypeConstructor("Op", List(makeVariable("operation")(translateSort(prog.programCrdt.operationType)))),
      DatatypeConstructor("Qry", List(makeVariable("query")(translateSort(prog.programCrdt.queryType)))),
      DatatypeConstructor("NoCall", List())
    )

    SortDatatypeImpl("callInfo", constructors.withKey(_.name))
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
          case UnitType() =>
            List()
          case rt =>
            List(makeVariable(name + "_arg")(translateSort(rt)))
        }

      val constr = DatatypeConstructor(name, args)
      name -> constr
    }
      .toMap
    val noInvoc = DatatypeConstructor("NoResult", List())

    SortDatatypeImpl("invocationResult", constructors + (noInvoc.name -> noInvoc))
  }

  def operationType: SymbolicSort = translateType(prog.programCrdt.operationType)(this)
  def queryType: SymbolicSort = translateType(prog.programCrdt.queryType)(this)


}

object SymbolicContext {

  sealed abstract class SolverResult {
    def isUnknown: Boolean = this == Unknown

  }

  case class Unsatisfiable(unsatCore: List[NamedConstraint]) extends SolverResult

  case object Unknown extends SolverResult

  case class Satisfiable(isIncomplete: Boolean, model: Model) extends SolverResult {

  }

  trait Model {
    /** evaluates a symbolic value to a string representation */
    def evaluate[T <: SymbolicSort](expr: SVal[T]): SVal[T]

  }


}



