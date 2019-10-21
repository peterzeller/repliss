package crdtver.symbolic

import crdtver.language.InputAst.NoSource
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper.{exists, _}
import crdtver.language.{TypedAst, TypedAstHelper}
import crdtver.utils.ListExtensions._
import crdtver.utils.MapUtils._

import scala.collection.mutable.ListBuffer

class ShapeAnalysis {

  def inferInvariants(prog: InProgram): InProgram = {
    val shapes = (for (p <- prog.procedures) yield p -> analyzeProc(p)).toMap
    val newInvariants = shapesToInvariants(shapes)
    println("SHAPE INVARIANTS: ")
    for (i <- newInvariants) {
      println(i)
      println("\n")
    }
    prog.copy(invariants = prog.invariants ++ newInvariants)
  }


  sealed abstract class Shape

  case class ShapeAlternative(left: Shape, right: Shape) extends Shape

  case class ShapeSequence(shapes: List[Shape]) extends Shape

  case class ShapeTransaction(
    calls: List[ShapeCall]
  ) extends Shape

//  case class Shape(
//    transactions: List[List[ShapeCall]]
//  ) {
//
//  }

  case class ShapeCall(
    procName: String,
    args: List[AbstractValue]
  )

  sealed abstract class AbstractValue {
    def typ: InTypeExpr
  }

  case class ParamValue(paramName: String, typ: InTypeExpr) extends AbstractValue

  case class AnyValue(name: String, typ: InTypeExpr) extends AbstractValue

  case class BoolValue(value: Boolean) extends AbstractValue {
    def typ: BoolType = TypedAst.BoolType()
  }

  var nameCounter = 0

  def newName(): String = {
    nameCounter += 1
    s"v_$nameCounter"
  }

  /** computes the union  */
  private def unionValues(a: AbstractValue, b: AbstractValue): AbstractValue = (a, b) match {
    case (x, y) if x == y => x
    case _ => AnyValue(newName(), a.typ)
  }

  def unionKnowledge(j: JoinInput[Boolean, Boolean]): Option[Boolean] = j match {
    case OnlyLeft(left) => None
    case OnlyRight(right) => None
    case Both(left, right) =>
      if (left == right) Some(left)
      else None
  }


  case class Context(
    varValues: Map[ProgramVariable, AbstractValue] = Map(),
  ) {
    def withKnowledge(av: AbstractValue, value: Boolean): Context =
      copy(varValues = for ((p, v) <- varValues) yield
        p -> (if (v == av) BoolValue(value) else v))

    def union(other: Context): Context = {
      Context(
        varValues = varValues.merge(other.varValues, unionValues)
      )

    }

    def withVar(name: Identifier, v: AbstractValue): Context =
      copy(varValues = varValues + (ProgramVariable(name.toString) -> v))

  }


  def analyzeProc(proc: TypedAst.InProcedure): Shape = {
    println(s"ANALYZE ${proc.name}")
    val ctxt = Context(
      varValues = proc.params.map(p => ProgramVariable(p.name.toString) -> ParamValue(p.name.toString, p.typ)).toMap
    )

    val (_finalCtxt, shape) = analyzeStmt(proc.body, ctxt)

    shape
  }

  private def extractCalls(s: Shape): List[List[ShapeCall]] = s match {
    case ShapeAlternative(left, right) =>
      extractCalls(left) ++ extractCalls(right)
    case ShapeSequence(shapes) =>
      var res: List[List[ShapeCall]] = List(List())
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

  private def analyzeStmt(stmt: TypedAst.InStatement, ctxt: Context): (Context, Shape) = {
//    println(s"  Analyze ${stmt.toString.replaceAll("\n", "")}")
//    println(s"    context = $ctxt")
    analyzeStmt2(stmt, ctxt)
  }

  private def analyzeStmt2(stmt: TypedAst.InStatement, ctxt: Context): (Context, Shape) = stmt match {
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
      (ctxt.withVar(variable.name, AnyValue(newName(), variable.typ)), emptyShape)

    case TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      val condV = evaluate(cond, ctxt)
      println(s"if stmt $cond --> $condV")
      val (c1, s1) = analyzeStmt(thenStmt, ctxt.withKnowledge(condV, true))
      val (c2, s2) = analyzeStmt(elseStmt, ctxt.withKnowledge(condV, false))
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
      (ctxt.withVar(varname, AnyValue(newName(), typename)), emptyShape)
    case TypedAst.ReturnStmt(source, expr, assertions) =>
      (ctxt, emptyShape)
    case TypedAst.AssertStmt(source, expr) =>
      (ctxt, emptyShape)
  }

  private def evaluate(expr: TypedAst.InExpr, ctxt: Context): AbstractValue = expr match {
    case TypedAst.VarUse(source, typ, name) =>
      ctxt.varValues(ProgramVariable(name))
    case TypedAst.BoolConst(source, typ, value) =>
      AnyValue(newName(), typ)
    case TypedAst.IntConst(source, typ, value) =>
      AnyValue(newName(), typ)
    case expr: TypedAst.CallExpr =>
      AnyValue(newName(), expr.getTyp)
    case TypedAst.QuantifierExpr(source, typ, quantifier, vars, expr) =>
      AnyValue(newName(), typ)
    case TypedAst.InAllValidSnapshots(expr) =>
      AnyValue(newName(), expr.getTyp)
  }


  private def shapesToInvariants(shapes: Map[InProcedure, Shape]): List[TypedAst.InInvariantDecl] = {


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

    val procShapeInvariants: List[TypedAst.InInvariantDecl] =
      for ((proc, shape) <- shapes.toList) yield {

        val paths = flattenShape(shape)

        val invoc = getVariable("invoc", InvocationIdType())

        val invocParams: Map[String, InVariable] =
          (for (p <- proc.params) yield (p.name.toString, getVariable(s"param_${p.name}", p.typ))).toMap

        val pathInits: List[List[ShapeTransaction]] =
          paths.flatMap(fullPath => fullPath.inits).distinct.sortBy(l => l.map(_.calls.size).sum)

        val alternatives: List[InExpr] =
          for (path <- pathInits) yield {
            val txns: List[InVariable] =
              for ((tx, i) <- path.zipWithIndex) yield
                getVariable(s"tx_$i", TransactionIdType())
            val tx = getVariable("tx", TransactionIdType())

            val callShapes: List[ShapeCall] =
              path.flatMap(tx => tx.calls)

            val calls: List[InVariable] =
              for ((tx, i) <- callShapes.zipWithIndex) yield
                getVariable(s"c_$i", CallIdType())

            val callToTx: List[(InVariable, InVariable)] =
              (for ((tx, txVar) <- path.zip(txns); c <- tx.calls) yield (txVar, c)).zip(calls).map(x => (x._2, x._1._1))

            val txToCalls: Map[InVariable, List[InVariable]] =
              callToTx.groupBy(_._2).view.mapValues(x => x.map(_._1)).toMap


            val c = getVariable("c", CallIdType())


            exists(txns,
              calculateAnd(
                List(
                  // txns are distinct
                  TypedAstHelper.distinct(txns.map(varUse)),

                  // all transactions from invocation are one of txns:
                  forall(tx,
                    implies(
                      isEquals(getOrigin(varUse(tx)), varUse(invoc)),
                      calculateOr(txns.map(t => isEquals(varUse(t), varUse(tx))))
                    )
                  ),
                  // there exist all the right calls:
                  exists(calls,
                    calculateAnd(
                      // calls are distinct
                      List(TypedAstHelper.distinct(calls.map(varUse)))
                        ++
                        // all calls from transaction are one of these
                        (for ((tx, calls) <- txToCalls) yield
                          forall(c,
                            implies(
                              isEquals(getOrigin(varUse(c)), varUse(tx)),
                              calculateOr(calls.map(cc => isEquals(varUse(c), varUse(cc)))))))
                        // origin for each call
                        ++
                        (for ((c, tx) <- callToTx) yield isEquals(getOrigin(varUse(c)), varUse(tx)))
                        // operation for each call:
                        ++
                        (for ((cs, c) <- callShapes.zip(calls)) yield {
                          val abstractVars = ListBuffer[InVariable]()
                          val args =
                            for ((a, i) <- cs.args.zipWithIndex) yield {
                              a match {
                                case ParamValue(paramName, typ) =>
                                  varUse(invocParams(paramName))
                                case AnyValue(name, typ) =>
                                  val v = getVariable(s"arg_$i", typ)
                                  abstractVars.addOne(v)
                                  varUse(v)
                                case BoolValue(b) =>
                                  TypedAstHelper.bool(b)
                              }
                            }

                          exists(abstractVars.toList, isEquals(getOp(varUse(c)), makeOperationL(cs.procName, args)))
                        })
                        // happens-before order between calls
                        ++
                        (for ((c1, c2) <- calls.pairs) yield happensBeforeCall(varUse(c1), varUse(c2)))

                    )
                  )
                )
              )
            )

          }


        TypedAst.InInvariantDecl(
          NoSource(),
          s"shape_of_invocation_${proc.name}",
          true,
          forall(invoc :: invocParams.values.toList,
            implies(
              isEquals(
                invocationInfo(varUse(invoc)),
                makeInvocationInfo(proc.name.toString, proc.params.map(p => varUse(invocParams(p.name.toString)))
                )
              ),
              calculateOr(alternatives)))
        )

      }



    // TODO possible origins for each kind of call

    procShapeInvariants
  }

  private def flattenShape(shape: Shape): List[List[ShapeTransaction]] = shape match {
    case ShapeAlternative(left, right) =>
      flattenShape(left) ++ flattenShape(right)
    case ShapeSequence(shapes) =>
      var res: List[List[ShapeTransaction]] = List(List())
      for (s <- shapes) {
        res = for {
          l <- res
          r <- flattenShape(s)
        } yield (l ++ r)
      }
      res
    case t: ShapeTransaction =>
      List(List(t))

  }


}
