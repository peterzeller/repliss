package crdtver.symbolic

import java.util.concurrent.TimeUnit

import crdtver.language.TypedAst
import crdtver.language.TypedAst._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.smt._
import crdtver.utils.ListExtensions._
import crdtver.utils.{ConcurrencyUtils, myMemo}

import scala.concurrent.duration.Duration

case class NamedConstraint(description: String, constraint: SVal[SortBoolean])

class SymbolicContext(
  val smtTranslation: ToSmtTranslation,
  val currentProcedure: String,
  prog: InProgram
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
      Smt.NamedConstraint(nc.description, smtTranslation.translateBool(nc.constraint))

  /**
    * Checks a list of constraints for satisfiability.
    *
    */
  def check(contraints: List[NamedConstraint], baseName: String, explainResult: Boolean): SolverResult = {
    val rLimit = ResourceLimit(1000000)
    val tLimit = SmtTimeout(Duration.apply(2, TimeUnit.MINUTES))
    val limits = List(rLimit, tLimit)

    val sharedOptions = limits ++
      (if (explainResult) List(SmtBuildUnsatCore(), SmtBuildModel()) else List())

    // try different options until a good solution (not 'unknown') is found
    val variants: List[(String, List[SmtOption])] =
        List(
          baseName -> sharedOptions,
          s"$baseName-fmf" -> (sharedOptions ++ List(FiniteModelFind()))
        )


    val translatedConstraints: List[Smt.NamedConstraint] = translateConstraints(contraints)

    import scala.concurrent.ExecutionContext.Implicits.global
    val results: List[ConcurrencyUtils.Task[SolverResult]] =
      for ((name, options) <- variants) yield {
        ConcurrencyUtils.spawn(
          name = name,
          work = () => {
            checkWithOptions(contraints, translatedConstraints, options, name)
          })
      }
    try {
      val firstResult: Option[SolverResult] = ConcurrencyUtils.race(results).find(!_.isUnknown)
      firstResult.getOrElse(SymbolicContext.Unknown)
    } finally {
      // cancel remaining executions
      results.foreach(_.cancel())
    }
  }

  private def checkWithOptions(contraints: List[NamedConstraint], translatedConstraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): SolverResult = {
    val solver: smt.Solver = new Z3Solver()
    val checkRes = solver.check(translatedConstraints, options, name)
    debugPrint("check: " + checkRes)
    checkRes match {
      case solver.Unsatisfiable(unsatCore) =>
        val unsatCoreOrig: List[NamedConstraint] = contraints.filter(c => unsatCore.exists(c2 => c.description == c2.description))
        SymbolicContext.Unsatisfiable(unsatCoreOrig)
      case solver.Unknown() =>
        SymbolicContext.Unknown
      case s: solver.Satisfiable =>
        Satisfiable(new Model {
          val m: solver.Model = s.getModel

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
    val smtConstraints: List[Smt.NamedConstraint] = for (c <- constraints) yield
      Smt.NamedConstraint(c.description, smtTranslation.translateExpr(c.constraint))
    val solver: smt.Solver = new Z3Solver()
    solver.exportConstraints(smtConstraints)
  }

  def exportConstraintsToSmt(constraints: List[NamedConstraint]): String = {
    val smtConstraints: List[Smt.NamedConstraint] = for (c <- constraints) yield
      Smt.NamedConstraint(c.description, smtTranslation.translateExpr(c.constraint))
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
      DatatypeConstructor("operation", List(makeVariable("operation")( translateSort(prog.programCrdt.operationType)))),
      DatatypeConstructor("query", List(makeVariable("query")( translateSort(prog.programCrdt.queryType)))),
      DatatypeConstructor("no_call", List())
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


}

object SymbolicContext {

  sealed abstract class SolverResult {
    def isUnknown: Boolean = this == Unknown

  }

  case class Unsatisfiable(unsatCore: List[NamedConstraint]) extends SolverResult

  case object Unknown extends SolverResult

  case class Satisfiable(model: Model) extends SolverResult

  trait Model {
    /** evaluates a symbolic value to a string representation */
    def evaluate[T <: SymbolicSort](expr: SVal[T]): SVal[T]

  }


}



