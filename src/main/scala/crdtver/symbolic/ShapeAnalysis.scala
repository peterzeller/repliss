package crdtver.symbolic

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{Identifier, InProcedure}
import crdtver.utils.MapUtils._

object ShapeAnalysis {

  sealed abstract class Shape

  case class ShapeAlternative(left: Shape, right: Shape) extends Shape

  case class ShapeSequence(shapes: List[Shape]) extends Shape

  case class ShapeTransaction(
    calls: List[ShapeCall]
  ) extends Shape

  case class ShapeCall(
    procName: String,
    args: List[AbstractValue]
  )

  sealed abstract class AbstractValue

  case class ParamValue(paramName: String) extends AbstractValue

  object AnyValue extends AbstractValue


  /** computes the union  */
  private def unionValues(a: AbstractValue, b: AbstractValue): AbstractValue = (a,b) match {
    case (ParamValue(p1), ParamValue(p2)) if p1 == p2 => AnyValue
    case _ => AnyValue
  }


  case class Context(
    varValues: Map[ProgramVariable, AbstractValue] = Map()
  ) {
    def union(other: Context): Context = {
      Context(
        varValues = varValues.merge(other.varValues, unionValues)
      )

    }

    def withVar(name: Identifier, v: AbstractValue): Context =
      copy(varValues = varValues + (ProgramVariable(name.toString) -> v))

  }


  def analyzeProc(proc: TypedAst.InProcedure): Shape = {
    val ctxt = Context(
      varValues = proc.params.map(p => ProgramVariable(p.name.toString) -> ParamValue(p.name.toString)  ).toMap,
    )

    val (_, shape) = analyzeStmt(proc.body, ctxt)
    shape
  }

  private def extractCalls(s: Shape): List[List[ShapeCall]] = s match {
      case ShapeAlternative(left, right) =>
        extractCalls(left) ++ extractCalls(right)
      case ShapeSequence(shapes) =>
        var res = List(List())
        for (s <- shapes) {
          res = for {
            l <- res
            r <- extractCalls(s)
          } yield (l ++ r)

        }
        res
      case ShapeTransaction(calls) =>
        List(calls)
  }

  private def shapeTransaction(s: Shape): Shape = {
    extractCalls(s).map(ShapeTransaction).reduceOption(ShapeAlternative).getOrElse(ShapeTransaction(List()))
  }

  private def emptyShape: Shape = ShapeSequence(List())

  private def analyzeStmt(stmt: TypedAst.InStatement, ctxt: Context): (Context, Shape) = stmt match {
    case TypedAst.BlockStmt(source, stmts) =>
      var c = ctxt
      val shapes: List[Shape] =
        for (stmt <- stmts) yield {
          val (c2, s) = analyzeStmt(stmt, c)
          c = c2
          s
        }
      (c, ShapeSequence(shapes))
    case TypedAst.Atomic(source, body) =>
      val (c2, s) = analyzeStmt(body, ctxt)
      (c2, shapeTransaction(s))

    case TypedAst.LocalVar(source, variable) =>
      (ctxt.withVar(variable.name, AnyValue), emptyShape)

    case TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      val (c1, s1) = analyzeStmt(thenStmt, ctxt)
      val (c2, s2) = analyzeStmt(elseStmt, ctxt)
      (c1.union(c2), ShapeAlternative(s1, s2))
    case TypedAst.MatchStmt(source, expr, cases) =>
      ???
    case TypedAst.CrdtCall(source, call) =>
      val args = call.args.map(evaluate(_, ctxt))
      (ctxt, ShapeTransaction(
        List(ShapeCall(call.functionName.toString, args))
      ))
    case TypedAst.Assignment(source, varname, expr) =>
      (ctxt.withVar(varname, evaluate(expr, ctxt)), emptyShape)
    case TypedAst.NewIdStmt(source, varname, typename) =>
      (ctxt.withVar(varname, AnyValue), emptyShape)
    case TypedAst.ReturnStmt(source, expr, assertions) =>
      (ctxt, emptyShape)
    case TypedAst.AssertStmt(source, expr) =>
      (ctxt, emptyShape)
  }

  private def evaluate(expr: TypedAst.InExpr, ctxt: Context): AbstractValue = expr match {
    case TypedAst.VarUse(source, typ, name) =>
      ctxt.varValues(ProgramVariable(name))
    case TypedAst.BoolConst(source, typ, value) =>
      AnyValue
    case TypedAst.IntConst(source, typ, value) =>
      AnyValue
    case expr: TypedAst.CallExpr =>
      AnyValue
    case TypedAst.QuantifierExpr(source, typ, quantifier, vars, expr) =>
      AnyValue
    case TypedAst.InAllValidSnapshots(expr) =>
      AnyValue
  }


  private def shapesToInvariants(shapes: Map[InProcedure, Shape]): List[TypedAst.InInvariantDecl] = {
    // flatten shapes --> all paths

    // shape invariants for procedures:
    // assume we have the following paths
    // tx1 -> tx2 -> tx3
    // tx1 -> tx4
    // then we have for all invocations
    // either: no transaction
    // or 1 transaction with shape of tx1
    // or 2 transactions with shape of tx1 then tx2
    // or 3 transactions with shape of tx1 then tx2 then tx3
    // or 2 transactions with shape of tx1 then tx4


    // possible origins for each kind of call

    ???
  }



}
