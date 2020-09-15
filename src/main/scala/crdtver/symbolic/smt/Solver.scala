package crdtver.symbolic.smt

import java.lang.Double.parseDouble
import java.time.Duration

import crdtver.symbolic.smt.Smt.{OpaqueExpr, QuantifierExpr, SmtExpr, SmtExprNode, Variable}
import crdtver.symbolic.smt.Solver.CheckRes
import crdtver.utils.MapUtils.MapExtensions
import crdtver.utils.myMemo

import scala.collection.mutable

/**
 * Effectively stateless solver.
 *
 * Might use internal caches to improve performance for iterative queries (first calling check(ys)
 * and later check(xs++ys) could be optimized using push and pop on a stateful smt solver in the background)
 *
 */
trait Solver {
  def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption] = List(), name: String): CheckRes

  def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption] = List()): String


}

object Solver {

  //  private class SolverParser extends RegexParsers {
  //
  //    def cvc4: Parser[Solver] = "cvc4" ^^ { _ => new Cvc4Solver() }
  //
  //    def cvc4fmf: Parser[Solver] = "cvc4f" ^^ { _ => new Cvc4Solver(finiteModelFind = true) }
  //
  //    def z3: Parser[Solver] = "z3" ^^ { _ => new Z3Solver() }
  //
  //    def group: Parser[Solver] = "(" ~> solver <~ ")"
  //
  //    def seq: Parser[Solver] = (solver2 ~ rep(";" ~> solver2)) ^^ { case (a ~ as) => new SequentialSolver(a :: as) }
  //
  //    def par: Parser[Solver] = (solver2 ~ rep("|" ~> solver2)) ^^ { case (a ~ as) => new ParallelSolver(a :: as) }
  //
  ////    def parseDigit: Parser[String] = "[0-9]".r
  ////
  ////    def float: Parser[Double] = rep(parseDigit) ~ "." ~ rep1(parseDigit) ^^ { case a ~ _ ~ c =>
  ////      parseDouble(a.mkString("") + "." + c.mkString(""))
  ////    }
  //
  //    def float: Parser[Double] = "[0-9]*\\.[0-9]+".r ^^ parseDouble
  //
  //    def timed: Parser[Solver] = (float ~ atomic) ^^ { case t ~ a => new TimeoutModSolver(a, t) }
  //
  //    def iterative: Parser[Solver] = ("I" ~> solver)
  //
  //    def atomic: Parser[Solver] =
  //      cvc4 | cvc4fmf | z3 | group
  //
  //    def solver2: Parser[Solver] =
  //      timed | iterative | atomic
  //
  //    def solver: Parser[Solver] =
  //      seq | par | solver2
  //
  //
  //    def parseSolver(s: String): Solver = {
  //      val r: ParseResult[Solver] = parse(solver, s.replaceAll("\\s+", ""))
  //      r match {
  //        case Success(result, _) => result
  //        case Failure(msg, rest) => throw new Exception(s"Failed to parse '$s' at ${rest.pos}:\n" + msg)
  //        case Error(msg, _) => throw new Exception(s"Error when parsing '$s': " + msg)
  //      }
  //    }
  //
  //  }

  private object SolverParser {
    import crdtver.utils.ParserCombinators._

    def cvc4: Parser[Solver] = "cvc4" ^^ { _ => new Cvc4Solver() }

    def cvc4fmf: Parser[Solver] = "cvc4f" ^^ { _ => new Cvc4Solver(finiteModelFind = true) }

    def z3: Parser[Solver] = "z3" ^^ { _ => new Z3Solver() }

    def group: Parser[Solver] = "(" ~> rec("solver", solver) <~ ")"


    def seq: Parser[Solver] = (rec("atomic", atomic) ~ rep(";" ~> rec("atomic", atomic))) ^^ { case (a, as) =>
      if (as.isEmpty) a else new SequentialSolver(a :: as) }

    def par: Parser[Solver] = (rec("seq", seq) ~ rep("|" ~> rec("seq", seq))) ^^ { case (a, as) =>
      if (as.isEmpty) a else new ParallelSolver(a :: as) }

    //    def parseDigit: Parser[String] = "[0-9]".r
    //
    //    def float: Parser[Double] = rep(parseDigit) ~ "." ~ rep1(parseDigit) ^^ { case a ~ _ ~ c =>
    //      parseDouble(a.mkString("") + "." + c.mkString(""))
    //    }

    def float: Parser[Double] = "[0-9]*\\.[0-9]+".r ^^ parseDouble

    def timed: Parser[Solver] = (float ~ atomic) ^^ { case (t, a) => new TimeoutModSolver(a, t) }

    def iterative: Parser[Solver] = ("I" ~> rec("solver", solver))

    def atomic: Parser[Solver] =
      cvc4fmf | cvc4 | z3 | group

    def solver2: Parser[Solver] =
      timed | iterative | atomic

    def solver: Parser[Solver] =
      par | seq | solver2

    def top: Parser[Solver] =
      solver ~ EOF

    def parseSolver(s: String): Solver = {
          val r: ParseResult[Solver] = top.parse(Input(s.replaceAll("\\s+", ""), 0))
          r match {
            case Success(result, _) => result
            case Fail(pos, msg) => throw new Exception(s"Failed to parse '$s' at '${s.substring(pos)}':\n" + msg)
          }
        }


  }

  def parseSolver(s: String): Solver =
    SolverParser.parseSolver(s)


  sealed abstract class CheckRes() {
    override def toString: String = this match {
      case satisfiable: Satisfiable =>
        "Satisfiable" + (if (satisfiable.isIncomplete) "(incomplete)" else "")
      case Unknown() =>
        "Unknown"
      case Unsatisfiable(unsatCore) =>
        "Unsatisfiable"
    }

    def isUnknown: Boolean = this match {
      case satisfiable: Satisfiable =>
        satisfiable.isIncomplete
      case Unknown() => true
      case Unsatisfiable(unsatCore) =>
        false
    }

  }

  abstract class Satisfiable() extends CheckRes() {
    def getModel: Model

    /** model might not be a complete model */
    def isIncomplete: Boolean
  }

  case class Unknown() extends CheckRes()

  case class Unsatisfiable(
    unsatCore: List[Smt.NamedConstraint]
  ) extends CheckRes()

  trait Model {
    def eval(expr: SmtExpr, bool: Boolean = true): SmtExpr

    // tries to get the universe
    protected def getUniverseIntern(typ: Smt.Type): Option[Set[SmtExpr]]

    def getConstraints: List[Smt.NamedConstraint]

    private lazy val allConstants: Set[Smt.Variable] = {
      val buf = new mutable.HashSet[Variable]()

      def collect(e: Smt.SmtExpr, bound: Set[Smt.Variable]): Unit = {
        e match {
          case q: QuantifierExpr =>
            collect(q.expr, bound + q.variable)
          case v: Smt.Variable =>
            if (!bound.contains(v))
              buf.addOne(v)
          case n: SmtExprNode =>
            for (c <- n.children)
              collect(c, bound)
          case _ =>

        }
      }

      for (c <- getConstraints) {
        collect(c.constraint, Set())
      }
      println(s"All constants = ${buf.mkString(", ")}")
      buf.toSet
    }

    private lazy val groundTerms: Map[Smt.Type, Set[Smt.SmtExpr]] = {
      val buf = new mutable.HashSet[Smt.SmtExpr]()

      def collect(e: SmtExpr): Unit = {
        e match {
          case n: Smt.SmtExprNode =>
            n.children.foreach(collect)
          case o: OpaqueExpr =>
            buf.addOne(o)
          case v: Smt.Variable =>
            buf.addOne(v)
          case _ =>
        }
      }


      for (c <- allConstants) {
        collect(eval(c))
      }
      buf.toSet.groupBy(_.calcType)
    }

    private val getUniverse: (Smt.Type) => Set[SmtExpr] = new myMemo[Smt.Type, Set[SmtExpr]]({ typ =>
      getUniverseIntern(typ).getOrElse {
        groundTerms.getOrElse(typ, {
          // TODO handle composed types
          // if the type does not exist, just create a singleton universe:
          val randomValue = eval(Smt.Variable(s"default_${typ}", typ))
          Set(randomValue)
        })

      }
    })

    /** like eval, but also tries to evaluate quantifiers */
    final def evalQ(expr: SmtExpr, bool: Boolean = true): Option[Boolean] = {
      require(expr.calcType == Smt.BoolType())

      def evalB(expr: SmtExpr)(implicit vars: Map[Smt.Variable, Smt.SmtExpr]): Boolean = {
        try {
          if (Thread.currentThread().isInterrupted)
            throw new InterruptedException()
          if (expr.containsQuantifiers) {
            expr match {
              case Smt.Equals(left, right) if left.calcType == Smt.BoolType() =>
                evalB(left) == evalB(right)
              case Smt.Not(of) =>
                !evalB(of)
              case Smt.IfThenElse(cond, ifTrue, ifFalse) if ifTrue.calcType == Smt.BoolType() =>
                if (evalB(cond)) evalB(ifTrue) else evalB(ifFalse)
              case Smt.QuantifierExpr(quantifier, variable, expr) =>
                val u = getUniverse(variable.typ)
                quantifier match {
                  case Smt.Forall() =>
                    u.forall(uv => evalB(expr)(vars + (variable -> uv)))
                  case Smt.Exists() =>
                    u.exists(uv => evalB(expr)(vars + (variable -> uv)))
                }

              case Smt.And(left, right) =>
                evalB(left) && evalB(right)
              case Smt.Or(left, right) =>
                evalB(left) || evalB(right)
              case Smt.Implies(left, right) =>
                !evalB(left) || evalB(right)
              case Smt.Const(b) => b
              case _ => eval(expr.subst(vars), bool) == Smt.Const(true)
            }
          } else {
            // no quantifiers --> use simple evaluation
            eval(expr.subst(vars), bool) == Smt.Const(true)
          }
        } catch {
          case exc: Throwable =>
            throw new Exception(s"Error evaluating expression $expr", exc)
        }
      }


      Some(evalB(expr)(Map()))
    }
  }

}

sealed abstract class SmtOption {

}

case class SmtTimeout(duration: Duration) extends SmtOption

case class ResourceLimit(limit: Int) extends SmtOption

case class SmtBuildModel() extends SmtOption

case class SmtBuildUnsatCore() extends SmtOption