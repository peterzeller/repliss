package crdtver.symbolic.smt

import crdtver.symbolic._
import crdtver.symbolic.smt.Smt.{Exists, Forall, SmtExpr}
import crdtver.utils.ProcessUtils
import edu.nyu.acsys.CVC4.{DatatypeConstructor, _}
import scalaz.Memo

/**
  *
  */
class Cvc4Solver(
  checkSatCmd: Boolean = true
) extends Solver {

  System.loadLibrary("cvc4jni")



  override def check(assertions: List[Smt.NamedConstraint]): CheckRes = {
    val instance = new Instance()
    val smt = instance.smt
    // TODO push pop optimization
    for (e <- assertions) {
      smt.assertFormula(instance.translateExpr(e.constraint)(instance.Context()))
    }
    val res = smt.checkSat()
    val sat: Result.Sat = res.isSat
    res.isSat match {
      case Result.Sat.UNSAT =>
        Unsatisfiable()
      case Result.Sat.SAT_UNKNOWN =>
        Unknown()
      case Result.Sat.SAT =>
        // is it really? verify with exported constraints ...
        if (checkSatCmd && !isSatCmd(assertions)) {
//          println(SmtPrinter.printScala(assertions.reverse))
          throw new RuntimeException("Different results on CMD and API")
        }

        new Satisfiable {

          override def getModel: Model = new Model {

            override def toString: String = {
              "satisfiable model"
            }

          override def eval(expr: SmtExpr, bool: Boolean): SmtExpr = {
            val r = smt.getValue(instance.translateExpr(expr)(instance.Context()))
            instance.parseExpr(r)
          }
        }
      }
    }
  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint]): String = {
    val instance = new Instance()
    instance.exportConstraints(assertions)
  }

  private def isSatCmd(assertions: List[Smt.NamedConstraint]): Boolean = {
    val cvc4in = exportConstraints(assertions)
    val res = ProcessUtils.runCommand(List("cvc4"), cvc4in)
    println(s"cvc4.stdout = '${res.stdout}'")
    println(s"cvc4.stderr = '${res.stderr}'")
    !res.stdout.contains("unsat")
  }


  private class Instance() {
    private var variables: Map[String, Expr] = Map()
    private var datatypes: List[Smt.Datatype] = List()
    private var sortTypes: List[Smt.Sort] = List()

    val emIntern: ExprManager = new ExprManager()
    val em: ExprManagerI = Cvc4Proxy.exprManager(emIntern)
    val smt: SmtEngineI = init()

    def translate() {}


    private def init(): SmtEngineI = {
      val smt: SmtEngineI = Cvc4Proxy.smtEngine(emIntern)
      smt.setOption("produce-models", Cvc4Proxy.SExpr(true))
      smt.setOption("finite-model-find", Cvc4Proxy.SExpr(true))
      smt.setOption("e-matching", Cvc4Proxy.SExpr(true))
      smt.setOption("incremental", Cvc4Proxy.SExpr(true))
      smt.setOption("tlimit", Cvc4Proxy.SExpr(30000))
      smt.setOption("produce-assertions", Cvc4Proxy.SExpr(true))
      smt.setOption("output-language", Cvc4Proxy.SExpr("cvc4")); // Set the output-language to CVC's
      smt.setOption("default-dag-thresh", Cvc4Proxy.SExpr(0)); //Disable dagifying the output
      smt
    }

    private def toVectorExpr(exprs: Iterable[Expr]): vectorExpr = {
      val r = new vectorExpr()
      for (e <- exprs) {
        r.add(e)
      }
      r
    }

    def translateType(typ: Smt.Type): Type = typ match {
      case t: Smt.Sort =>
        translateSort(t)
      case dt: Smt.Datatype =>
        translateDatatype(dt)
      case Smt.IntegerType() =>
        em.integerType()
      case Smt.BoolType() =>
        em.booleanType()
      case Smt.ArrayType(keyType, valueType) =>
        em.mkArrayType(translateType(keyType), translateType(valueType))
      case Smt.SetType(elementType) =>
        em.mkSetType(translateType(elementType))
    }

    private val translateSort: Smt.Sort => SortType = Memo.mutableHashMapMemo[Smt.Sort, SortType](t => {
      sortTypes ::= t
      em.mkSort(t.name)
    })

    private val translateDatatype: Smt.Datatype => DatatypeType = Memo.mutableHashMapMemo[Smt.Datatype, DatatypeType](dt => {
      val rdt = Cvc4Proxy.Datatype(dt.name)
      for (c <- dt.constructors) {
        val rc = Cvc4Proxy.DatatypeConstructor(c.name)
        for (arg <- c.args) {
          Cvc4Proxy.addConstructorArg(rc, arg.name, translateType(arg.typ))
        }
        Cvc4Proxy.addConstructor(rdt, rc)
      }
      datatypes ::= dt
      em.mkDatatypeType(rdt)
    })

    private def getConstructorExpr(dt: Smt.Datatype, constructor: Smt.DatatypeConstructor): Expr = {
      val tdt: Datatype = translateDatatype(dt).getDatatype
      Cvc4Proxy.getConstructor(tdt, constructor.name)
    }


    private def getConstructor(dt: Smt.Datatype, constructor: Smt.DatatypeConstructor): DatatypeConstructor = {
      val tdt: Datatype = translateDatatype(dt).getDatatype
      tdt.get(constructor.name)
    }

    case class Context(
      boundVars: Map[String, Expr] = Map()
    ) {
      def withVariable(variable: Smt.Variable, v: Expr): Context = copy(boundVars = boundVars + (variable.name -> v))

    }

    def translateExpr(e: Smt.SmtExpr)(implicit ctxt: Context): Expr = e match {
      case node: Smt.SmtExprNode => node match {
        case Smt.Equals(left, right) =>
          em.mkExpr(Kind.EQUAL, translateExpr(left), translateExpr(right))
        case Smt.Not(of) =>
          em.mkExpr(Kind.NOT, translateExpr(of))
        case Smt.ApplyConstructor(dt, constructor, args) =>
          em.mkExpr(Kind.APPLY_CONSTRUCTOR, getConstructorExpr(dt, constructor), toVectorExpr(args.map(translateExpr)))
        case Smt.ApplySelector(dt, constructor, variable, expr) =>
          val selector = Cvc4Proxy.getSelector(getConstructor(dt, constructor), variable.name)
          em.mkExpr(Kind.APPLY_SELECTOR, selector, translateExpr(expr))
        case Smt.IfThenElse(cond, ifTrue, ifFalse) =>
          em.mkExpr(Kind.ITE, translateExpr(cond), translateExpr(ifTrue), translateExpr(ifFalse))
        case Smt.ApplyTester(dt, constructor, expr) =>
          val selector = Cvc4Proxy.getTester(getConstructor(dt, constructor))
          em.mkExpr(Kind.APPLY_TESTER, selector, translateExpr(expr))
        case Smt.MapSelect(map, key) =>
          em.mkExpr(Kind.SELECT, translateExpr(map), translateExpr(key))
        case Smt.ConstantMap(typ, defaultValue) =>
          em.mkConst(new ArrayStoreAll(translateType(typ).asInstanceOf[ArrayType], translateExpr(defaultValue)))
        case Smt.MapStore(map, key, newValue) =>
          em.mkExpr(Kind.STORE, translateExpr(map), translateExpr(key), translateExpr(newValue))
        case Smt.SetSingleton(value) =>
          em.mkExpr(Kind.SINGLETON, translateExpr(value))
        case Smt.SetInsert(set, vals) =>
          val lastSingle = em.mkExpr(Kind.SINGLETON, translateExpr(vals.last))
          if (vals.size == 1) {
            lastSingle
          } else {
            em.mkExpr(Kind.INSERT, toVectorExpr(vals.init.map(v => translateExpr(v)) ++ List(lastSingle)))
          }
        case Smt.Union(left, right) =>
          em.mkExpr(Kind.UNION, translateExpr(left), translateExpr(right))
        case Smt.Member(value, set) =>
          em.mkExpr(Kind.MEMBER, translateExpr(value), translateExpr(set))
        case Smt.QuantifierExpr(quantifier: Smt.Quantifier, variable, expr) =>
          val kind = quantifier match {
            case Forall() => Kind.FORALL
            case Exists() => Kind.EXISTS
          }
          val v = em.mkBoundVar(variable.name, translateType(variable.typ))
          val newContext = ctxt.withVariable(variable, v)
          em.mkExpr(kind, em.mkExpr(Kind.BOUND_VAR_LIST, v), translateExpr(expr)(newContext))
        case Smt.And(left, right) =>
          em.mkExpr(Kind.AND, translateExpr(left), translateExpr(right))
        case Smt.Or(left, right) =>
          em.mkExpr(Kind.OR, translateExpr(left), translateExpr(right))
        case Smt.Implies(left, right) =>
          em.mkExpr(Kind.IMPLIES, translateExpr(left), translateExpr(right))
        case Smt.IsSubsetOf(left, right) =>
          em.mkExpr(Kind.SUBSET, translateExpr(left), translateExpr(right))
        case Smt.SetContains(elem, set) =>
          em.mkExpr(Kind.MEMBER, translateExpr(elem), translateExpr(set))
        case Smt.Leq(left, right) =>
          em.mkExpr(Kind.LEQ, translateExpr(left), translateExpr(right))
        case Smt.Lt(left, right) =>
          em.mkExpr(Kind.LT, translateExpr(left), translateExpr(right))
      }
      case Smt.Variable(name, typ) =>
        ctxt.boundVars.get(name) match {
          case Some(value) =>
            value
          case None =>
            // must be a global variable
            variables.get(name) match {
              case Some(value) =>
                value
              case None =>
                val v = em.mkVar(name, translateType(typ))
                variables += (name -> v)
                v
            }
        }
      case Smt.Const(b) =>
        em.mkConst(b)
      case Smt.ConstI(i) =>
        em.mkConst(new Rational(i.bigInteger.toString))
      case Smt.EmptySet(valueType) =>
        em.mkConst(Cvc4Proxy.mkEmptySet(translateType(valueType).asInstanceOf[SetType]))
      case Smt.Distinct(elems) =>
        if (elems.size < 2)
          em.mkConst(true)
        else
          em.mkExpr(Kind.DISTINCT, toVectorExpr(elems.map(translateExpr)))
      case Smt.OpaqueExpr(kind, expr) =>
        expr.asInstanceOf[Expr]
    }


    var indent = 0

    def debugPrint(str: String): Unit = {

    }

    private def parseType(st: Type): Smt.Type = {
      st match {
        case at: ArrayType =>
          Smt.ArrayType(parseType(at.getIndexType), parseType(at.getConstituentType))
        case t: SetType =>
          Smt.SetType(parseType(t.getElementType))
        case _ =>
          if (st.isSort) {
            sortTypes.find(_.name == st.toString).getOrElse(throw new RuntimeException(s"Sort type $st not found in $sortTypes"))
          } else if (st.isDatatype) {
            datatypes.find(_.name == st.toString).getOrElse(throw new RuntimeException(s"Datatype $st not found in $datatypes"))
          } else if (st.isSet) {
            parseType(st.castToSet())
          } else {
            throw new RuntimeException(s"Unhandled case: $st (${st.getClass})")
          }
      }
    }


    def parseExpr(expr: Expr): SmtExpr = {
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
            parseExpr(children(0)),
            parseExpr(children(1)),
            parseExpr(children(2)))
        case Kind.STORE_ALL =>
          val const = expr.getConstArrayStoreAll
          Smt.ConstantMap(parseType(const.getType), parseExpr(const.getExpr))
        case Kind.SINGLETON =>
          Smt.SetSingleton(parseExpr(children(0)))
        case Kind.EMPTYSET =>
          Smt.EmptySet(parseType(expr.getConstEmptySet.getType.getElementType))
        case Kind.UNION =>
          Smt.Union(parseExpr(children(0)), parseExpr(children(1)))
        case Kind.APPLY_CONSTRUCTOR =>
          val constructorName = expr.getOperator.toString
          val dt = datatypes.find(dt => dt.constructors.exists(c => c.name == constructorName)).getOrElse(throw new RuntimeException(s"Constructor $constructorName not found in $datatypes"))
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
          c.description -> translateExpr(c.constraint)(Context())
        }

      for (st: Smt.Sort <- sortTypes.reverse) {
        append(s"${st.name}: TYPE;\n")
      }
      for (dt: Smt.Datatype <- datatypes.reverse) {
        append(s"DATATYPE ${dt.name} = \n")
        for ((c, i) <- dt.constructors.zipWithIndex) {
          if (i > 0) {
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
              append(s"${arg.name}: ${translateType(arg.typ)}")
            }
            append(")")
          }
          append("\n")
        }
        append("END;\n")
      }


      for ((v, t) <- variables) {
        append(s"$v: ${t.getType};\n")
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
//      append("COUNTERMODEL;\n")
//      append("COUNTEREXAMPLE;\n")
      append("\n")
      r.toString()
    }
  }


}

