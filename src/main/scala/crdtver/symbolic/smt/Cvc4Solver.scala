package crdtver.symbolic.smt

import java.util

import crdtver.symbolic.smt.Smt.SmtExpr
import crdtver.symbolic._
import edu.nyu.acsys.CVC4._

import scala.collection.Set

/**
  *
  */
class Cvc4Solver extends Solver {

  private var variables: Map[String, Type] = Map()
  private var usedVarNames: Set[String] = Set()
  private var datatypes: Map[String, Smt.Datatype] = Map()
  private var sortTypes: Map[String, Smt.Sort] = Map()

  System.loadLibrary("cvc4jni")

  val emIntern: ExprManager = new ExprManager()
  val em: ExprManagerI = Cvc4Proxy.exprManager(emIntern)






  // TODO push pop optimization


  override def check(expression: List[Smt.SmtExpr]): CheckRes = {
    val smt: SmtEngineI = Cvc4Proxy.smtEngine(emIntern)
    smt.setOption("produce-models", Cvc4Proxy.SExpr(true))
    smt.setOption("finite-model-find", Cvc4Proxy.SExpr(true))
    smt.setOption("e-matching", Cvc4Proxy.SExpr(true))
    smt.setOption("incremental", Cvc4Proxy.SExpr(true))
    smt.setOption("tlimit", Cvc4Proxy.SExpr(30000))
    smt.setOption("produce-assertions", Cvc4Proxy.SExpr(true))
    smt.setOption("output-language", Cvc4Proxy.SExpr("cvc4")); // Set the output-language to CVC's
    smt.setOption("default-dag-thresh", Cvc4Proxy.SExpr(0)); //Disable dagifying the output
    for (e <- expression) {
      smt.assertFormula(translateExpr(e))
    }
    val res = smt.checkSat()
    val sat: Result.Sat = res.isSat
    if (sat == Result.Sat.UNSAT) {
      Unsatisfiable()
    } else if (sat == Result.Sat.SAT_UNKNOWN) {
      Unknown()
    } else {
      new Satisfiable {

        override def getModel: Model = new Model {

          override def toString: String = {
            "satisfiable model"
          }

          override def eval(expr: Smt.SmtExpr, bool: Boolean): Smt.SmtExpr = {
            val r = smt.getValue(translateExpr(expr))
            parseExpr(r)
          }
        }
      }
    }
  }

  private def toVectorExpr(exprs: Iterable[Expr]): vectorExpr = {
    val r = new vectorExpr()
    for (e <- exprs) {
      r.add(e)
    }
    r
  }

  def translateExpr(e: Smt.SmtExpr): Expr = e match {
    case node: Smt.SmtExprNode => node match {
      case Smt.Equals(left, right) =>
        em.mkExpr(Kind.EQUAL, translateExpr(left), translateExpr(right))
      case Smt.Not(of) =>
        em.mkExpr(Kind.NOT, translateExpr(of))
      case Smt.ApplyConstructor(dt, constructor, args@_*) =>
        em.mkExpr(Kind.APPLY_CONSTRUCTOR, getConstructor(dt, constructor), toVectorExpr(args.map(translateExpr)))
      case Smt.ApplySelector(dt, constructor, variable, expr) =>
      case Smt.IfThenElse(cond, ifTrue, ifFalse) =>
      case Smt.ApplyTester(dt, constructor, expr) =>
      case Smt.MapSelect(map, key) =>
      case Smt.ConstantMap(defaultValue) =>
      case Smt.MapStore(map, key, newValue) =>
      case Smt.SetSingleton(value) =>
      case Smt.SetInsert(set, values) =>
      case Smt.Union(left, right) =>
      case Smt.Member(value, set) =>
      case Smt.QuantifierExpr(quantifier, variable, expr) =>
      case Smt.And(left, right) =>
      case Smt.Or(left, right) =>
      case Smt.Implies(left, right) =>
      case Smt.IsSubsetOf(left, right) =>
      case Smt.Leq(left, right) =>
      case Smt.Lt(left, right) =>
    }
    case Smt.Variable(name, typ) =>
    case Smt.Const(b) =>
    case Smt.ConstI(i) =>
    case Smt.EmptySet(valueType) =>
    case Smt.Distinct(elems) =>
    case Smt.OpaqueExpr(kind, expr) =>
  }


  var indent = 0

  def debugPrint(str: String): Unit = {

  }

  private def parseType(st: Type): Smt.Type =
    throw new RuntimeException(s"Unhandled case: $st (${st.getClass}")

  private def parseExpr(expr: Expr): SmtExpr = {
    indent += 1
    val kind = expr.getKind

    lazy val children: List[Expr] =
      (for (i <- 0L until expr.getNumChildren) yield expr.getChild(i)).toList

    debugPrint(s"translate $expr")
    //    debugPrint(s"targetType = $t")
    debugPrint(s"kind = $kind")
    //    debugPrint(s"children = $children")

    val result: Smt.SmtExpr = kind match {
      case Kind.STORE =>
        Smt.MapStore(
          parseExpr(children(1)),
          parseExpr(children(2)),
          parseExpr(children(0)))
      case Kind.STORE_ALL =>
        val const = expr.getConstArrayStoreAll
        Smt.ConstantMap(parseExpr(const.getExpr))
      case Kind.SINGLETON =>
        Smt.SetSingleton(parseExpr(children(0)))
      case Kind.EMPTYSET =>
        Smt.EmptySet(parseType(expr.getConstEmptySet.getType.getElementType))
      case Kind.UNION =>
        Smt.Union(parseExpr(children(0)), parseExpr(children(1)))
      case Kind.APPLY_CONSTRUCTOR =>
        val constructorName = expr.getOperator.toString
        val dt = datatypes.values.find(dt => dt.constructors.exists(c => c.name == constructorName)).getOrElse(throw new RuntimeException(s"Constructor $constructorName not found in $datatypes"))
        val args: List[SmtExpr] = (0L until expr.getNumChildren).map(i => parseExpr(expr.getChild(i))).toList
        Smt.ApplyConstructor(dt, constructorName, args)
      case _ =>
        debugPrint(s"opaque with kind $kind")
        Smt.OpaqueExpr(kind, expr)
    }
    debugPrint(s"--> $result")

    indent -= 1
    result
  }


  /**
    * export a list of constraints to the CVC4 input language
    */
  def exportConstraints(constraints: List[Smt.NamedConstraint]): String = {
    val r = new StringBuilder()

    def append(o: Any): Unit = {
      r.append(o)
    }

    append(
      """
        |OPTION "finite-model-find" TRUE;
        |OPTION "produce-models" TRUE;
      """.stripMargin)

    val constraints2: List[(String, Expr)] =
      for (c <- constraints) yield {
        c.description -> translateExpr(c.constraint)
      }

    for (st: Smt.Sort <- sortTypes.values) {
      append(s"${st.name}: TYPE;\n")
    }
    for (dt: Smt.Datatype <- datatypes.values) {
      append(s"DATATYPE ${dt.name} = \n")
      for ((c,i) <- dt.constructors.zipWithIndex) {
        if (i == 0) {
          append(" | ")
        } else {
          append("   ")
        }
        append(s"${c.name}")
        if (c.args.nonEmpty) {
          append("(")
          for ((arg, j) <- c.args.zipWithIndex) {
            if (j > 0) {
              append(", ")
            }
            append(s"${arg.name}: ${arg.typ}")
          }
          append(")")
        }
        append("\n")
      }
      append("END;\n")
    }


    for ((v, t) <- variables) {
      append(s"$v: $t;\n")
    }

    for ((name, a) <- constraints2) {
      append("\n")
      append(s"% ${name.replaceAll("\n", "\n% ")}\n")
      //
      //      val os = new ByteArrayOutputStream()
      //
      //      a.toStream(os, Int.MaxValue, true, 1000L, OutputLanguage.OUTPUT_LANG_TPTP)
      //
      //      val r = os.toString(StandardCharsets.UTF_8)
      //
      //      append(s"%----\n% ${r.replaceAll("\n", "\n% ")}\n")

      append(s"ASSERT $a;\n")
    }


    append("CHECKSAT;\n")
    append("COUNTERMODEL;\n")
    append("COUNTEREXAMPLE;\n")
    append("\n")
    r.toString()
  }

}

