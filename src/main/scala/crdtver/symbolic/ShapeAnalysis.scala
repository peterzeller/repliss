package crdtver.symbolic

import java.nio.MappedByteBuffer

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.FunctionKind.FunctionKindDatatypeConstructor
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper.{existsL, _}
import crdtver.language.crdts.CrdtTypeDefinition
import crdtver.language.{InputAst, TypedAst, TypedAstHelper}
import crdtver.utils.LazyListUtils
import crdtver.utils.ListExtensions._
import crdtver.utils.MapUtils._
import crdtver.utils.PrettyPrintDoc.{Doc, nested}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ShapeAnalysis {

  def paramToVariable(param: CrdtTypeDefinition.Param): InVariable =
    getVariable(param.name, param.typ)

  def inferInvariants(prog: InProgram): InProgram = {
    val shapes = (for (p <- prog.procedures) yield p -> analyzeProc(p)).toMap
    val newInvariants = shapesToInvariants(shapes, Map(), prog)
    for (inv <- newInvariants; if inv.name == "shape_of_invocation_editMessage") {
      println(inv.printAst.prettyStr(80))
    }
    System.exit(1)
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
    op: AbstractValue
  ) {

    def subst(av: AbstractValue, bv: AbstractValue): ShapeCall =
      copy(op = op.subst(av, bv))

  }

  case class MatchSubst(
    leftSubst: Map[AnyValue, AbstractValue],
    rightSubst: Map[AnyValue, AbstractValue]
  ) {
    def withLeft(v: AnyValue, x: AbstractValue): MatchSubst =
      copy(leftSubst = leftSubst + (v -> x))

    def withRight(v: AnyValue, x: AbstractValue): MatchSubst =
      copy(rightSubst = rightSubst + (v -> x))

    def leftSubstG: Map[AbstractValue, AbstractValue] =
      Map[AbstractValue, AbstractValue]() ++ leftSubst

    def rightSubstG: Map[AbstractValue, AbstractValue] =
      Map[AbstractValue, AbstractValue]() ++ rightSubst
  }

  sealed abstract class AbstractValue {
    def substTop(s: Map[AbstractValue, AbstractValue]): AbstractValue = {
      s.getOrElse(this, this)
    }

    /** tries to match this vs other
     *
     * */
    def matchWith(other: AbstractValue): Option[MatchSubst] = {

      def m(a: AbstractValue, b: AbstractValue, s: MatchSubst): Option[MatchSubst] = {
        (a.substTop(s.leftSubstG), b.substTop(s.rightSubstG)) match {
          case (ParamValue(paramName1, typ1), ParamValue(paramName2, typ2))
            if paramName1 == paramName2
              && typ1 == typ2 =>
            Some(s)
          case (v: AnyValue, x) =>
            Some(s.withLeft(v, x))
          case (x, v: AnyValue) =>
            Some(s.withRight(v, x))
          case (BoolValue(b1), BoolValue(b2)) if b1 == b2 =>
            Some(s)
          case (DatatypeValue(typ1, name1, args1), DatatypeValue(typ2, name2, args2))
            if typ1 == typ2
              && name1 == name2
              && args1.length == args2.length
          =>
            var subst = s
            for ((a1, a2) <- args1.zip(args2)) {
              m(a1, a2, subst) match {
                case Some(newSubst) =>
                  subst = newSubst
                case None =>
                  return None
              }
            }
            Some(subst)
          case _ =>
            None
        }
      }

      m(this, other, MatchSubst(Map(), Map()))
    }

    def subst(s: Map[AbstractValue, AbstractValue]): AbstractValue = {
      s.get(this) match {
        case Some(v) => v
        case None =>
          this match {
            case dv: DatatypeValue =>
              dv.copy(args = dv.args.map(_.subst(s)))
            case _ => this
          }
      }

    }

    def subst(av: AbstractValue, bv: AbstractValue): AbstractValue =
      subst(Map(av -> bv))

    def typ: InTypeExpr

    def toInExpr(invocParams: Map[String, InVariable]): (Map[AnyValue, InVariable], InExpr) = {
      var vars = Map[AnyValue, InVariable]()
      var i = 0

      def toExpr(av: AbstractValue): InExpr = {
        av match {
          //                          case ParamValue(paramName, typ) =>
          //                            varUse(invocParams(paramName))
          //                          case AnyValue(name, typ) =>
          //                            val v = getVariable(s"arg_$i", typ)
          //                            abstractVars.addOne(v)
          //                            varUse(v)
          //                          case BoolValue(b) =>
          //                            TypedAstHelper.bool(b)
          case ParamValue(paramName, typ) =>
            varUse(invocParams(paramName))
          case av@AnyValue(name, typ) =>
            vars.get(av) match {
              case Some(value) => varUse(value)
              case None =>
                val v = getVariable(s"arg_${i}_$name", typ)
                i += 1
                vars += (av -> v)
                varUse(v)
            }

          case BoolValue(b) =>
            TypedAstHelper.bool(b)
          case DatatypeValue(typ, constructorName, args) =>
            TypedAst.FunctionCall(
              NoSource(),
              typ = typ, functionName = Identifier(NoSource(), constructorName),
              typeArgs = List(), args = args.map(toExpr), kind = FunctionKindDatatypeConstructor()
            )
        }
      }

      val e = toExpr(this)
      (vars, e)
    }

    def toDoc: Doc = {
      import crdtver.utils.PrettyPrintDoc._

      this match {
        case ParamValue(paramName, typ) =>
          "$param(" <> paramName <> ")"
        case AnyValue(name, typ) =>
          "#" <> name
        case BoolValue(value) =>
          value.toString
        case DatatypeValue(typ, constructorName, args) =>
          group(constructorName <> "(" <> nested(2, line <> sep(", ", args.map(_.toDoc))) <> ")")
      }
    }

    override def toString: String =
      toDoc.prettyStr(120)
  }

  case class ParamValue(paramName: String, typ: InTypeExpr) extends AbstractValue

  case class AnyValue(name: String, typ: InTypeExpr) extends AbstractValue

  case class BoolValue(value: Boolean) extends AbstractValue {
    def typ: BoolType = TypedAst.BoolType()
  }

  case class DatatypeValue(typ: InTypeExpr, constructorName: String, args: List[AbstractValue]) extends AbstractValue {

  }

  var nameCounter = 0

  def newName(): String = {
    nameCounter += 1
    s"v_$nameCounter"
  }

  /** computes the union */
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

  def matchPattern(pattern: InExpr, exprV: AbstractValue, ctxt: Context): Option[Context] =
    pattern match {
      case v@VarUse(source, typ, name) =>
        Some(ctxt.withVar(Identifier(NoSource(), v.name), exprV))
      case FunctionCall(source, typ, functionName, typeArgs, args, kind) =>
        val aArgsO = exprV match {
          case DatatypeValue(typ, constructorName, dArgs) =>
            if (functionName.name == constructorName)
              Some(dArgs)
            else
              None
          case _ =>
            Some(args.map(a => AnyValue(newName(), a.getTyp)))
        }
        aArgsO.flatMap { aArgs =>
          var c = ctxt
          for ((pa, aa) <- args.zip(aArgs)) {
            matchPattern(pa, aa, c) match {
              case Some(newC) =>
                c = newC
              case None =>
                return None
            }
          }
          Some(c)
        }
      case _ =>
        throw new Exception("Invalid Pattern")
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
      val exprV = evaluate(expr, ctxt)
      for {
        c <- cases.to(LazyList)
        ctxt2 <- matchPattern(c.pattern, exprV, ctxt).to(LazyList)
        l <- analyzeStmt(c.statement, ctxt2)
      } yield l
    case TypedAst.CrdtCall(source, call) =>
      val op = evaluate(call, ctxt)
      LazyList(ctxt.withCall(ShapeCall(op)))
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
      expr match {
        case FunctionCall(_, typ, functionName, _, args, FunctionKindDatatypeConstructor()) =>
          DatatypeValue(typ, functionName.name,
            args.map(evaluate(_, ctxt)))
        case _ =>
          AnyValue(newName(), expr.getTyp)
      }

    case TypedAst.QuantifierExpr(source, quantifier, vars, expr) =>
      AnyValue(newName(), BoolType())
    case TypedAst.InAllValidSnapshots(_, expr) =>
      AnyValue(newName(), expr.getTyp)
    case TypedAst.CrdtQuery(source, typ, qryOp) =>
      AnyValue(newName(), expr.getTyp)
  }


  private def shapesToInvariants(procShapes: Map[InProcedure, List[Shape]], operations: Map[String, List[InVariable]], prog: InProgram): List[TypedAst.InInvariantDecl] = {


    makeProcShapeInvariants(procShapes, prog) ++
      makeReverseShapeInvariants(procShapes, prog)
  }


  def listAllOperationShapes(prog: InProgram): LazyList[ShapeCall] = {
    var i = 0

    def newVar(t: InTypeExpr): AnyValue = {
      i += 1
      AnyValue(s"x_$i", t)
    }

    def listT(t: InTypeExpr): LazyList[AbstractValue] = {
      t match {
        case SimpleType(name, typeArgs) =>
          list(prog.findType(name).getOrElse(throw new Exception(s"Could not find type $name")))
        case _ =>
          LazyList(newVar(t))
      }
    }

    def list(decl: InTypeDecl): LazyList[AbstractValue] = {
      val t = decl.toTypeExpr
      val nestedList: LazyList[AbstractValue] =
        if (decl.dataTypeCases.isEmpty)
          LazyList()
        else {
          for {
            c <- decl.dataTypeCases.to(LazyList)
            params <- LazyListUtils.allCombinations(c.params.map(p => listT(p.typ)))
          } yield {
            DatatypeValue(t, c.name.name, params)
          }
        }
      newVar(t) #:: nestedList
    }


    val opsAndQueries: LazyList[AbstractValue] = listT(prog.programCrdt.operationType).map(op => DatatypeValue(CallInfoType(), "Op", List(op))) ++
      listT(prog.programCrdt.queryType).map(op => DatatypeValue(CallInfoType(), "Qry", List(op)))

    opsAndQueries.map(ShapeCall)
  }

  /** from operation to procedure */
  private def makeReverseShapeInvariants(procShapes: Map[InProcedure, List[Shape]], prog: InProgram): List[InInvariantDecl] = {
    // collect the shapes


    /*
    TODO
    go through all shapes
    collect all the matching shapes from procShapes
    generate big OR with all the matches
    if possible, preserve invocation arguments in matches
     */

    // 1. operation shape,
    // 2. forall vars
    // 3. formula for op == ...
    // 4. formula for all possible procedure invocations with origin
    val invariants1 = new ListBuffer[(ShapeCall, List[InVariable], InExpr, InExpr)]()

    val c = "c" :: CallIdType()

    for (opShape <- listAllOperationShapes(prog)) {

      val (freeVars, shapeExpr) = opShape.op.toInExpr(Map())


      val procs = new ListBuffer[InExpr]()
      for {
        (proc, pShapes) <- procShapes
        pShape <- pShapes
        tShape <- pShape.transactions
        shape <- tShape.calls
      } {

        opShape.op.matchWith(shape.op) match {
          case Some(subst) =>


            val existsVars = new ListBuffer[InVariable]

            val args: List[InExpr] =
              for (p <- proc.params) yield {
                subst.leftSubst.find(x => x._2 == ParamValue(p.name.name, p.typ)) match {
                  case Some(v) =>
                    val rv = freeVars.getE(v._1)
                    varUse(rv)
                  case None =>
                    val newV = p.name.name :: p.typ
                    existsVars.addOne(newV)
                    varUse(newV)
                }
              }
            val invoc = "invoc" :: InvocationIdType()
            val expr = existsL(invoc :: existsVars.toList,
              varUse(c).origin === varUse(invoc) &&
                varUse(invoc).info === dtVal(proc.name.name, InvocationInfoType(), args))
            procs.addOne(expr)
          case None =>
        }
      }
      val right = calculateOr(procs.toList.distinct)
      invariants1.addOne((opShape, c :: freeVars.values.toList, shapeExpr, right))
    }

    // TODO remove redundant cases from invariants
    // if x is more precise than y and both have the same right hand side,
    // then only keep the more general y

    for ((shape, freeVars, shapeExpr, right) <- invariants1.toList) yield {


      val expr = forallL(freeVars, (varUse(c).op === shapeExpr) --> right)

      InInvariantDecl(
        NoSource(),
        s"shape_rev_${shape.op.toString.replaceAll("[^a-zA-Z0-9]+", "_")}",
        true,
        expr,
        priority = 150
      )
    }
  }


  /** from procedure to possible transactions and database operations */
  private def makeProcShapeInvariants(shapes: Map[InProcedure, List[Shape]], prog: InProgram): List[InInvariantDecl] = {
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
    for ((proc, shape) <- shapes.toList) yield {

      val paths: List[Shape] = shape

      val invoc = getVariable("invoc", InvocationIdType())

      val invocParams: Map[String, InVariable] =
        (for (p <- proc.params) yield (p.name.toString, getVariable(s"param_${p.name}", p.typ))).toMap

      val pathInits: List[List[ShapeTransaction]] =
        paths.flatMap(fullPath => fullPath.transactions.inits).distinct.sortBy(l => l.map(_.calls.size).sum)

      val alternatives: List[InExpr] =
        for (path <- pathInits) yield {
          val isFullPath = paths.contains(Shape(path))

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


          existsL(txns,
            calculateAnd(
              List(
                // if not the full path, then invocation cannot have a result
                if (!isFullPath)
                  varUse(invoc).result === NoResult()
                else
                  bool(true),

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
                existsL(calls, {
                  val facts = new ListBuffer[InExpr]()

                  // calls are distinct
                  facts.addOne(TypedAstHelper.distinct(calls.map(varUse)))

                  // all calls from transaction are one of these
                  facts.addAll(for ((tx, calls) <- txToCalls) yield
                    forall(c,
                      implies(
                        (getTransaction(varUse(c)) === varUse(tx)),
                        calculateOr(calls.map(cc => isEquals(varUse(c), varUse(cc)))))))

                  // origin for each call
                  facts.addAll(for ((c, tx) <- callToTx) yield isEquals(getTransaction(varUse(c)), varUse(tx)))

                  // operation for each call:
                  facts.addAll(for ((cs, c) <- callShapes.zip(calls)) yield {
                    val (abstractVars, op) = cs.op.toInExpr(invocParams)
                    existsL(abstractVars.values.toList, (getOp(varUse(c)) === op))
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
        forallL(invoc :: invocParams.values.toList,
          implies(
            isEquals(
              invocationInfo(varUse(invoc)),
              makeInvocationInfo(proc.name.toString, proc.params.map(p => varUse(invocParams(p.name.toString)))
              )
            ),
            calculateOr(alternatives))),
        priority = 10000
      )

    }
  }
}
