package crdtver.symbolic.smt

import com.microsoft.z3
import com.microsoft.z3.enumerations.Z3_decl_kind
import com.microsoft.z3.enumerations.Z3_decl_kind._
import com.microsoft.z3.enumerations.Z3_sort_kind._
import com.microsoft.z3.{Model => _, _}
import crdtver.symbolic.smt.Smt.{Div, Forall, Minus, Mod, Mult, NamedConstraint, Plus, SmtExpr}
import crdtver.symbolic.smt.Solver._
import crdtver.symbolic.smt
import crdtver.utils.Helper.unexpected
import crdtver.utils.{Helper, NativeUtils, myMemo}

import scala.collection.mutable.ListBuffer

object Z3Solver {
  private val lock = new Object
  private var loaded = false

  def loadLibrary(): Unit = {
    lock.synchronized {
      if (!loaded) {
        NativeUtils.loadLibraryFromJar("/native/libz3.so")
        NativeUtils.loadLibraryFromJar("/native/libz3java.so")
        loaded = true
      }
    }
  }
}

class Z3Solver extends smt.Solver {
  Z3Solver.loadLibrary()

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption], name: String): CheckRes = {
    val i = new Instance(options)
    val export = exportConstraints(constraints, options)
//    println(s"#### CHECK $name\n$export")
    i.solve(constraints)
  }

  private class Instance(options: List[SmtOption]) {
    val ctxt: Context = new Context
    val solver: z3.Solver = ctxt.mkSolver()

    private val solverParams = ctxt.mkParams()
    for (opt <- options) {
      // compare z3/src/cmd_context/context_params.cpp
      opt match {
        case FiniteModelFind() =>
        case ResourceLimit(limit) =>
          solverParams.add("rlimit", limit)
        case SmtBuildModel() =>
          solverParams.add("model", true)
        case SmtBuildUnsatCore() =>
          solverParams.add("unsat_core", true)
        case SmtTimeout(duration) =>
          solverParams.add("timeout", duration.toMillis.toInt)
      }
    }
    solver.setParameters(solverParams)


    val uninterpretedSortTrans = new myMemo[Smt.Sort, UninterpretedSort]({ s =>
      ctxt.mkUninterpretedSort(s.name)
    })

    val datatypeTrans = new myMemo[Smt.Datatype, DatatypeSort]({ d =>

      val constructors = for (c <- d.constructors) yield {
        val fieldNames = c.args.map(_.name).toArray
        val fieldSorts = c.args.map(a => translateType(a.typ)).toArray
        val fieldSortRefs = c.args.map(_ => 0).toArray

        ctxt.mkConstructor(c.name, s"is_${c.name}", fieldNames, fieldSorts, fieldSortRefs)
      }

      ctxt.mkDatatypeSort(d.name, constructors.toArray)
    })

    val variableTrans = new myMemo[Smt.Variable, Expr]({ d =>
      ctxt.mkConst(d.name, translateType(d.typ))
    })

    val funcDefTrans = new myMemo[Smt.FuncDef, FuncDecl]({ f =>
      ctxt.mkFuncDecl(f.name, f.args.map(translateType).toArray, translateType(f.returnType))
    })

    private def translateType(typ: Smt.Type): Sort = typ match {
      case s: Smt.Sort =>
        uninterpretedSortTrans(s)
      case d: Smt.Datatype =>
        datatypeTrans(d)
      case Smt.IntegerType() =>
        ctxt.mkIntSort()
      case Smt.BoolType() =>
        ctxt.mkBoolSort()
      case Smt.ArrayType(keyType, valueType) =>
        ctxt.mkArraySort(translateType(keyType), translateType(valueType))
      case Smt.SetType(elementType) =>
        ctxt.mkSetSort(translateType(elementType))
    }

    private case class TrContext(
      boundVars: List[Smt.Variable] = List()
    ) {
      def withBoundVar(v: Smt.Variable): TrContext =
        copy(boundVars = v :: boundVars)
    }

    private def translateExpr(e: NamedConstraint): BoolExpr =
      translateExprBool(e.constraint)(TrContext())

    private var symbolCount = 0

    private def makeSymbol: IntSymbol = {
      symbolCount += 1
      ctxt.mkSymbol(symbolCount)
    }


    private def translateExprBool(e: SmtExpr)(implicit trCtxt: TrContext): BoolExpr =
      translateExpr(e).asInstanceOf[BoolExpr]

    private def translateExprArith(e: SmtExpr)(implicit trCtxt: TrContext): ArithExpr =
      translateExpr(e).asInstanceOf[ArithExpr]

    private def translateExprArray(e: SmtExpr)(implicit trCtxt: TrContext): ArrayExpr =
      translateExpr(e).asInstanceOf[ArrayExpr]


    private def translateExpr(e: SmtExpr)(implicit trCtxt: TrContext): Expr = try {
      e match {
        case node: Smt.SmtExprNode =>
          node match {
            case Smt.Equals(left, right) =>
              ctxt.mkEq(translateExpr(left), translateExpr(right))
            case Smt.Not(of) =>
              ctxt.mkNot(translateExprBool(of))
            case Smt.ApplyConstructor(dt, constructor, args) =>
              val d = datatypeTrans(dt)
              val i = dt.constructors.indexOf(constructor)
              val f = d.getConstructors()(i)
              ctxt.mkApp(f, args.map(translateExpr): _*)
            case Smt.ApplySelector(dt, constructor, variable, expr) =>
              val d = datatypeTrans(dt)
              val ci = dt.constructors.indexOf(constructor)
              val vi = constructor.args.indexOf(variable)
              val f = d.getAccessors()(ci)(vi)
              ctxt.mkApp(f, translateExpr(expr))
            case Smt.ApplyTester(dt, constructor, expr) =>
              val d = datatypeTrans(dt)
              val ci = dt.constructors.indexOf(constructor)
              val f = d.getRecognizers()(ci)
              ctxt.mkApp(f, translateExpr(expr))
            case Smt.IfThenElse(cond, ifTrue, ifFalse) =>
              ctxt.mkITE(translateExprBool(cond), translateExpr(ifTrue), translateExpr(ifFalse))
            case Smt.MapSelect(map, key) =>
              ctxt.mkSelect(translateExprArray(map), translateExpr(key))
            case Smt.ConstantMap(keyType, defaultValue) =>
              ctxt.mkConstArray(translateType(keyType), translateExpr(defaultValue))
            case Smt.MapStore(map, key, newValue) =>
              ctxt.mkStore(translateExprArray(map), translateExpr(key), translateExpr(newValue))
            case Smt.SetSingleton(value) =>
              ctxt.mkSetAdd(ctxt.mkEmptySet(translateType(value.calcType)), translateExpr(value))
            case Smt.SetInsert(set, values) =>
              values.foldRight(translateExprArray(set))((x, acc) =>
                ctxt.mkSetAdd(acc, translateExpr(x)))
            case Smt.Union(left, right) =>
              ctxt.mkSetUnion(translateExprArray(left), translateExprArray(right))
            case Smt.QuantifierExpr(quantifier, v, body) =>
              val sorts = Array(translateType(v.typ))
              val names: Array[z3.Symbol] = Array(ctxt.mkSymbol(v.name).asInstanceOf[z3.Symbol])

              ctxt.mkQuantifier(
                quantifier == Forall(),
                sorts,
                names,
                translateExpr(body)(trCtxt.withBoundVar(v)),
                0,
                Array[Pattern](),
                Array[Expr](),
                makeSymbol,
                makeSymbol)
            case Smt.And(left, right) =>
              ctxt.mkAnd(translateExprBool(left), translateExprBool(right))
            case Smt.Or(left, right) =>
              ctxt.mkOr(translateExprBool(left), translateExprBool(right))
            case Smt.Implies(left, right) =>
              ctxt.mkImplies(translateExprBool(left), translateExprBool(right))
            case Smt.IsSubsetOf(left, right) =>
              ctxt.mkSetSubset(translateExprArray(left), translateExprArray(right))
            case Smt.SetContains(element, set) =>
              ctxt.mkSetMembership(translateExpr(element), translateExprArray(set))
            case Smt.Leq(left, right) =>
              ctxt.mkLe(translateExprArith(left), translateExprArith(right))
            case Smt.Lt(left, right) =>
              ctxt.mkLt(translateExprArith(left), translateExprArith(right))
            case Plus(left, right) =>
              ctxt.mkAdd(translateExprArith(left), translateExprArith(right))
            case Minus(left, right) =>
              ctxt.mkSub(translateExprArith(left), translateExprArith(right))
            case Mod(left, right) =>
              ctxt.mkMod(translateExprArith(left).asInstanceOf[IntExpr], translateExprArith(right).asInstanceOf[IntExpr])
            case Mult(left, right) =>
              ctxt.mkMul(translateExprArith(left), translateExprArith(right))
            case Div(left, right) =>
              ctxt.mkDiv(translateExprArith(left), translateExprArith(right))

            case Smt.ApplyFunc(f, args) =>
              val fd = funcDefTrans(f)
              ctxt.mkApp(fd, args.map(translateExpr): _*)
            case Smt.Distinct(elems) =>
              ctxt.mkDistinct(elems.map(translateExpr): _*)
          }
        case v: Smt.Variable =>
          trCtxt.boundVars.indexOf(v) match {
            case i if i >= 0 =>
              ctxt.mkBound(i, translateType(v.typ))
            case _ =>
              variableTrans(v)
          }
        case Smt.Const(b) =>
          ctxt.mkBool(b)
        case Smt.ConstI(i) =>
          ctxt.mkInt(i.toString())
        case Smt.EmptySet(valueType) =>
          ctxt.mkEmptySet(translateType(valueType))
        case Smt.OpaqueExpr(kind, expr) =>
          expr.asInstanceOf[Expr]
      }
    } catch {
      case exc: Exception =>
        throw new Exception(s"Error when translating expression $e", exc)
    }

    private def parseSort(s: Sort): Smt.Type = {
      s.getSortKind match {
        case Z3_UNINTERPRETED_SORT =>
          Smt.Sort(s.getName.toString)
        case Z3_BOOL_SORT =>
          Smt.BoolType()
        case Z3_INT_SORT =>
          Smt.IntegerType()
        case Z3_REAL_SORT => ???
        case Z3_BV_SORT => ???
        case Z3_ARRAY_SORT =>
          val a = s.asInstanceOf[ArraySort]
          Smt.ArrayType(parseSort(a.getDomain), parseSort(a.getRange))
        case Z3_DATATYPE_SORT =>
          val d = s.asInstanceOf[DatatypeSort]
          val constructors = for (c <- d.getConstructors.toList) yield {
            val params = c.getParameters.zip(c.getDomain)
              .map(p => Smt.Variable(p._1.getSymbol.toString, parseSort(p._2))).toList
            Smt.DatatypeConstructor(c.getName.toString, params)
          }
          Smt.Datatype(d.getName.toString, constructors)
        case Z3_RELATION_SORT => ???
        case Z3_FINITE_DOMAIN_SORT => ???
        case Z3_FLOATING_POINT_SORT => ???
        case Z3_ROUNDING_MODE_SORT => ???
        case Z3_SEQ_SORT => ???
        case Z3_RE_SORT => ???
        case Z3_UNKNOWN_SORT => ???
      }
    }

    sealed trait ParseLambdaResult {

      import ParseLambdaResult._

      def toSmtExpr(expected: Smt.Type): SmtExpr = expected match {
        case Smt.ArrayType(keyType, valueType) =>
          this match {
            case SimpleExpr(e) =>
              Smt.ConstantMap(keyType, e)
            case IfThenElse(Smt.Equals(l, r), SimpleExpr(e), ifFalse) =>
              Smt.MapStore(ifFalse.toSmtExpr(expected), r, e)
            case other =>
              throw new Exception(s"unhandled case $other")
          }
        case Smt.SetType(elementType) =>
          this match {
            case SimpleExpr(Smt.Equals(l, r)) =>
              Smt.SetSingleton(r)
            case SimpleExpr(Smt.Or(l, r)) =>
              Smt.Union(SimpleExpr(l).toSmtExpr(expected), SimpleExpr(r).toSmtExpr(expected))
            case other =>
              throw new Exception(s"unhandled case $other")
          }
        case other =>
          throw new Exception(s"unhandled case $other")
      }

    }

    object ParseLambdaResult {


      case class SimpleExpr(e: SmtExpr) extends ParseLambdaResult

      case class IfThenElse(cond: SmtExpr, ifTrue: ParseLambdaResult, ifFalse: ParseLambdaResult) extends ParseLambdaResult

    }

    def parseSetOrMap(v: z3.Symbol, t: Sort, body: Expr, expected: Smt.Type): ParseLambdaResult = {
      if (body.isApp) {
        val f = body.getFuncDecl
        f.getDeclKind match {
          case Z3_OP_ITE =>
            return ParseLambdaResult.IfThenElse(
              parseExpr(body.getArgs()(0), Smt.BoolType()),
              parseSetOrMap(v, t, body.getArgs()(1), expected),
              parseSetOrMap(v, t, body.getArgs()(2), expected)
            )
          case _ =>
        }
      }
      ParseLambdaResult.SimpleExpr(parseExpr(body, expected))
    }

    def arrayValueType(t: Smt.Type): Smt.Type = t match {
      case Smt.ArrayType(keyType, valueType) => valueType
      case Smt.SetType(elementType) => Smt.BoolType()
      case _ =>
        throw new Exception(s"not an array or set type: $t")
    }


    private def parseExpr(e: Expr, expectedType: Smt.Type): SmtExpr = {
      try {
        if (e.isInt) {
          Smt.ConstI(Integer.parseInt(e.toString))
        } else if (e.isQuantifier) {
          val q = e.asInstanceOf[Quantifier]
          if (!q.isExistential && !q.isUniversal) {
            require(q.getNumBound == 1)
            parseSetOrMap(q.getBoundVariableNames()(0), q.getBoundVariableSorts()(0), q.getBody, arrayValueType(expectedType))
              .toSmtExpr(expectedType)
          } else {
            throw new Exception(s"Cannot parse quantifier expression $q  // ${q.isExistential} // ${q.isUniversal}")
          }
        } else if (e.isVar) {
          Smt.Variable(e.toString, parseSort(e.getSort))
        } else if (e.isEq) {
          Smt.Equals(parseExpr(e.getArgs()(0), Smt.BoolType()), parseExpr(e.getArgs()(1), Smt.BoolType()))
        } else if (e.isStore) {
          val args = e.getArgs
          expectedType match {
            case Smt.ArrayType(keyType, valueType) =>
              val map = parseExpr(args(0), expectedType)
              val key = parseExpr(args(1), keyType)
              val newValue = parseExpr(args(2), valueType)
              Smt.MapStore(map, key, newValue)
            case Smt.SetType(elementType) =>
              assert(parseExpr(args(2), Smt.BoolType()) == Smt.Const(true))
              Smt.SetInsert(parseExpr(args(0), expectedType), List(parseExpr(args(1), elementType)))
            case other =>
              throw new Exception(s"Unhandled case: $other")
          }
        } else if (e.isTrue) {
          Smt.Const(true)
        } else if (e.isFalse) {
          Smt.Const(false)
        } else if (e.isApp) {
          val f = e.getFuncDecl
          import Z3_decl_kind._
          f.getDeclKind match {
            case Z3_OP_CONST_ARRAY =>
              val arSort = e.getSort.asInstanceOf[ArraySort]
              expectedType match {
                case Smt.ArrayType(keyType, valueType) =>
                  Smt.ConstantMap(parseSort(arSort.getDomain), parseExpr(e.getArgs()(0), valueType))
                case Smt.SetType(elementType) =>
                  assert(parseExpr(e.getArgs()(0), Smt.BoolType()) == Smt.Const(false))
                  Smt.EmptySet(elementType)
                case other => unexpected(other)
              }
            case Z3_OP_DT_CONSTRUCTOR =>
              val dt: (Smt.Datatype, DatatypeSort) = datatypeTrans.find(x => x._2.getConstructors.contains(f)).getOrElse(
                throw new Exception(s"Could not find datatype constructor ${f.getName} in ${datatypeTrans.toList}")
              )
              val c = dt._1.constructors.find(_.name == f.getName.toString).get
              Smt.ApplyConstructor(dt._1, f.getName.toString,
                e.getArgs.zip(c.args).map(a => parseExpr(a._1, a._2.typ)).toList)
            case Z3_OP_UNINTERPRETED =>
              Smt.Variable(e.toString, parseSort(e.getSort))
            case other =>
              throw new Exception(s"Cannot parse expression $e of of type ${e.getClass} func type $other")
          }
        } else {
          throw new Exception(s"Cannot parse expression $e of type ${e.getClass}")
        }
      } catch {
        case exc: Exception =>
          throw new Exception(s"Error parsing expression $e", exc)
      }
    }

    def solve(expression: List[Smt.NamedConstraint]): CheckRes = {
      val translationsBuf = new ListBuffer[(NamedConstraint, BoolExpr)]()

      for (e <- expression) {
        val tr: BoolExpr = translateExpr(e)
        solver.add(tr)
        translationsBuf.addOne(e -> tr)
      }
      val translations = translationsBuf.toList

//      println("### Solver assertions:")
//      for (c <- solver.getAssertions) {
//        println(s"(asset $c)")
//      }
      val status = solver.check()
      status match {
        case Status.UNSATISFIABLE =>
          val unsatCore: List[NamedConstraint] =
            if (options.contains(SmtBuildUnsatCore)) {
              solver.getUnsatCore.view
                .flatMap(e => translations.filter(p => p._2 == e))
                .map(_._1)
                .toList
            } else {
              List()
            }
          Unsatisfiable(unsatCore)
        case Status.UNKNOWN =>
          Unknown()
        case Status.SATISFIABLE =>
          new Satisfiable {

            override def isIncomplete: Boolean = false

            override def getModel: Model = {
              val model = solver.getModel
              new Model {
                override def eval(expr: SmtExpr, bool: Boolean): SmtExpr =
                  parseExpr(model.eval(translateExpr(expr)(TrContext()), bool), expr.calcType)

                override protected def getUniverseIntern(typ: Smt.Type): Option[Set[SmtExpr]] = {
                  try {
                    Some(model.getSortUniverse(translateType(typ)).map(parseExpr(_, typ)).toSet)
                  } catch {
                    case exc: Throwable =>
                      exc.printStackTrace()
                      None
                  }
                }


                override def getConstraints: List[NamedConstraint] = expression
              }
            }
          }
      }
    }
  }


  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption]): String =
    SmtLibPrinter.print(assertions).prettyStr(120)
}
