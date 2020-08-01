package crdtver.symbolic

import java.nio.MappedByteBuffer

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper.{exists, _}
import crdtver.language.crdts.CrdtTypeDefinition
import crdtver.language.{InputAst, TypedAst, TypedAstHelper}
import crdtver.utils.ListExtensions._
import crdtver.utils.MapUtils._

import scala.collection.mutable.ListBuffer

class ShapeAnalysis {

  def paramToVariable(param: CrdtTypeDefinition.Param): InVariable =
    getVariable(param.name, param.typ)

  def inferInvariants(prog: InProgram): InProgram = {
    val shapes = (for (p <- prog.procedures) yield p -> analyzeProc(p)).toMap

    //    val operations: Map[String, List[InVariable]] =
    //      (for (op <- prog.programCrdt.operations()) yield
    //        op.name -> op.params.map(paramToVariable)).toMap ++
    //        (for (op <- prog.programCrdt.queries()) yield
    //          s"queryop_${op.qname}" -> (op.params.map(paramToVariable) :+ getVariable("result", op.qreturnType))).toMap

    /* TODO change to new structure of operations

    instead of using the list of operations, go through the shapes, and collect the datatypes:
    variables + function to construct operation term

     */

    val newInvariants = shapesToInvariants(shapes, Map(), prog)
    prog.copy(invariants = prog.invariants ++ newInvariants)
  }


  case class Shape(
    transactions: List[ShapeTransaction]
  ) {
    def subst(av: AbstractValue, bv: AbstractValue): Shape =
      copy(transactions.map(_.subst(av, bv)))

    def inits: Iterator[Shape] = transactions.inits.map(Shape)

    def withCall(call: ShapeCall): Shape = copy(
      transactions.init :+ (transactions.last.withCall(call))
    )

    def withNewTx(): Shape = copy(transactions :+ ShapeTransaction())

  }

  case class ShapeTransaction(
    calls: List[ShapeCall] = List()
  ) {
    def subst(av: AbstractValue, bv: AbstractValue): ShapeTransaction =
      copy(calls.map(_.subst(av, bv)))

    def withCall(call: ShapeCall): ShapeTransaction = copy(
      calls :+ call
    )
  }

  case class ShapeCall(
    procName: String,
    args: List[AbstractValue]
  ) {
    def subst(av: AbstractValue, bv: AbstractValue): ShapeCall =
      copy(args = args.map(_.subst(av, bv)))

  }

  sealed abstract class AbstractValue {
    def subst(av: AbstractValue, bv: AbstractValue): AbstractValue =
      if (this == av) bv else this

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
    shape: Shape
  ) {
    def withCall(call: ShapeCall): Context = copy(
      shape = shape.withCall(call)
    )


    def newTxn: Context = copy(
      shape = shape.withNewTx()
    )

    def withKnowledge(av: AbstractValue, value: Boolean): Context = {
      val bv = BoolValue(value)
      copy(
        varValues = for ((p, v) <- varValues) yield
          p -> (if (v == av) bv else v),
        shape = shape.subst(av, bv)
      )
    }


    def withVar(name: Identifier, v: AbstractValue): Context =
      copy(varValues = varValues + (ProgramVariable(name.toString) -> v))

  }


  def analyzeProc(proc: TypedAst.InProcedure): List[Shape] = {
    val ctxt = Context(
      varValues = proc.params.map(p => ProgramVariable(p.name.toString) -> ParamValue(p.name.toString, p.typ)).toMap,
      shape = emptyShape
    )

    val finalCtxt = analyzeStmt(proc.body, ctxt)

    finalCtxt.map(_.shape).toList
  }

  private def extractCalls(s: Shape): List[List[ShapeCall]] =
    s.transactions.map(_.calls)


  private def emptyShape: Shape = Shape(List())

  private def analyzeStmts(stmts: List[InStatement], ctxt: Context): LazyList[Context] = stmts match {
    case List() =>
      LazyList(ctxt)
    case x :: xs =>
      for (c1 <- analyzeStmt(x, ctxt); c2 <- analyzeStmts(xs, c1)) yield c2

  }

  private def analyzeStmt(stmt: TypedAst.InStatement, ctxt: Context): LazyList[Context] = stmt match {
    case TypedAst.BlockStmt(source, stmts) =>
      analyzeStmts(stmts, ctxt)
    case TypedAst.Atomic(source, body) =>
      analyzeStmt(body, ctxt.newTxn)
    case TypedAst.LocalVar(source, variable) =>
      LazyList(ctxt.withVar(variable.name, AnyValue(newName(), variable.typ)))
    case TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      val condV = evaluate(cond, ctxt)
      analyzeStmt(thenStmt, ctxt.withKnowledge(condV, true)) ++
        analyzeStmt(elseStmt, ctxt.withKnowledge(condV, false))
    case TypedAst.MatchStmt(source, expr, cases) =>
      ???
    case TypedAst.CrdtCall(source, call) =>
      val args = call.args.map(evaluate(_, ctxt))
      LazyList(ctxt.withCall(ShapeCall(call.functionName.toString, args)))
    case TypedAst.Assignment(source, varname, expr) =>
      LazyList(ctxt.withVar(varname, evaluate(expr, ctxt)))
    case TypedAst.NewIdStmt(source, varname, typename) =>
      LazyList(ctxt.withVar(varname, AnyValue(newName(), typename)))
    case TypedAst.ReturnStmt(source, expr, assertions) =>
      LazyList(ctxt)
    case TypedAst.AssertStmt(source, expr) =>
      LazyList(ctxt)
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
    case TypedAst.QuantifierExpr(source, quantifier, vars, expr) =>
      AnyValue(newName(), BoolType())
    case TypedAst.InAllValidSnapshots(_, expr) =>
      AnyValue(newName(), expr.getTyp)
  }


  private def shapesToInvariants(procShapes: Map[InProcedure, List[Shape]], operations: Map[String, List[InVariable]], prog: InProgram): List[TypedAst.InInvariantDecl] = {


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

    val procShapeInvariants: List[InInvariantDecl] =
      makeProcShapeInvariants(procShapes, prog)



    // possible origins for each kind of call


    // operation name -> list of (freeVars in procArgs, procName, procArgs)
    val collectSources: Map[String, List[(List[InVariable], String, List[InExpr])]] = {
      val res = ListBuffer[(String, (List[InVariable], String, List[InExpr]))]()
      for {
        (proc, shapes) <- procShapes
        shape <- shapes
        tx <- shape.transactions
        call <- tx.calls
      } {
        val opParams: List[InVariable] = operations.getOrElse(call.procName,
          throw new RuntimeException(s"Could not find operation ${call.procName} in ${operations.keySet}"))

        var freeVars = ListBuffer[InVariable]()
        val args: List[InExpr] =
          for (p <- proc.params) yield {
            call.args.view.zip(opParams).flatMap {
              case (ParamValue(paramName, typ), opParam) if paramName == p.name.name =>
                Some(varUse(opParam))
              case _ =>
                None
            }.headOption.getOrElse {
              freeVars.addOne(p)
              varUse(p)
            }
          }

        res.addOne((call.procName, (freeVars.toList, proc.name.toString, args)))
      }
      res.toList.toMap2
    }

    val originInvariants =
      for ((opName, opParams) <- operations) yield {

        val c = TypedAstHelper.getVariable("c", CallIdType())
        val i = getVariable("i", InvocationIdType())

        TypedAst.InInvariantDecl(
          NoSource(),
          s"call_${opName}_origins",
          true,
          forall(c +: opParams,
            implies(
              isEquals(getOp(varUse(c)), makeOperationL(opName, prog.programCrdt.operationType, List(),  opParams.map(varUse))),
              exists(List(i),
                and(
                  isEquals(getOrigin(varUse(c)), varUse(i)),
                  calculateOr {
                    for (alts <- collectSources.get(opName).view; (freeVars, procName, args) <- alts.distinct) yield {
                      exists(freeVars,
                        isEquals(invocationInfo(varUse(i)), makeInvocationInfo(procName, args))
                      )
                    }
                  }
                )
              )
            )
          )
        )
      }


    procShapeInvariants ++ originInvariants
  }


  private def makeProcShapeInvariants(shapes: Map[InProcedure, List[Shape]], prog: InProgram): List[InInvariantDecl] = {
    for ((proc, shape) <- shapes.toList) yield {

      val paths: List[Shape] = shape

      val invoc = getVariable("invoc", InvocationIdType())

      val invocParams: Map[String, InVariable] =
        (for (p <- proc.params) yield (p.name.toString, getVariable(s"param_${p.name}", p.typ))).toMap

      val pathInits: List[List[ShapeTransaction]] =
        paths.flatMap(fullPath => fullPath.transactions.inits).distinct.sortBy(l => l.map(_.calls.size).sum)

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
                exists(calls, {
                  val facts = new ListBuffer[InExpr]()

                  // calls are distinct
                  facts.addOne(TypedAstHelper.distinct(calls.map(varUse)))

                  // all calls from transaction are one of these
                  facts.addAll(for ((tx, calls) <- txToCalls) yield
                    forall(c,
                      implies(
                        isEquals(getTransaction(varUse(c)), varUse(tx)),
                        calculateOr(calls.map(cc => isEquals(varUse(c), varUse(cc)))))))

                  // origin for each call
                  facts.addAll(for ((c, tx) <- callToTx) yield isEquals(getTransaction(varUse(c)), varUse(tx)))

                  // operation for each call:
                  facts.addAll(for ((cs, c) <- callShapes.zip(calls)) yield {
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

                    exists(abstractVars.toList, isEquals(getOp(varUse(c)), makeOperationL(cs.procName, prog.programCrdt.operationType, List(), args)))
                  })

                  // happens-before order between calls
                  facts.addAll(for ((c1, c2) <- calls.pairs) yield happensBeforeCall(varUse(c1), varUse(c2)))

                  calculateAnd(facts.toList)
                })
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
  }
}
