package crdtver.symbolic

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util

import crdtver.symbolic.Cvc4Proxy.getConstructor
import crdtver.utils.myMemo
import edu.nyu.acsys.CVC4._
import scalaz.Memo

import scala.collection.{Set, immutable}


/**
  *
  */
class Cvc4Translation(
  limitInvocations: Option[Int] = None,
  limitTransactions: Option[Int] = None,
  limitCalls: Option[Int] = None,
  limitCustomTypes: Option[Int] = None,
) extends SmtTranslation {
  override type TBoolExpr = Expr
  override type TExpr = Expr

  private var variables: Map[String, Type] = Map()
  private var usedVarNames: Set[String] = Set()

  System.loadLibrary("cvc4jni")

  val emIntern: ExprManager = new ExprManager()
  val em: ExprManagerI = Cvc4Proxy.exprManager(emIntern)


  def mkSolver(): SmtSolver = new SmtSolver() {

    val smt: SmtEngineI = Cvc4Proxy.smtEngine(emIntern)
    smt.setOption("produce-models", Cvc4Proxy.SExpr(true))
    smt.setOption("finite-model-find", Cvc4Proxy.SExpr(true))
    smt.setOption("e-matching", Cvc4Proxy.SExpr(true))
    smt.setOption("incremental", Cvc4Proxy.SExpr(true))
    smt.setOption("tlimit", Cvc4Proxy.SExpr(30000))
    smt.setOption("produce-assertions", Cvc4Proxy.SExpr(true))
    smt.setOption("output-language", Cvc4Proxy.SExpr("cvc4")); // Set the output-language to CVC's
    smt.setOption("default-dag-thresh", Cvc4Proxy.SExpr(0)); //Disable dagifying the output


    override def add(translated: Expr): Unit = {
      smt.assertFormula(translated)
      debugPrint(s"Assert formula:\n$translated")
    }

    override def check(): CheckRes = {
      val res = smt.checkSat()
      debugPrint(s"SAT result = $res")
      val sat: Result.Sat = res.isSat
      if (sat == Result.Sat.UNSAT) {
        Unsatisfiable()
      } else if (sat == Result.Sat.SAT_UNKNOWN) {
        debugPrint(s"unknown because ${res.whyUnknown()}")
        Unknown()
      } else {
        new Satisfiable {

          override def getModel: Model = new Model {
            override def eval(expr: Expr, bool: Boolean): Expr = {
              val r = smt.getValue(expr)
              val kind = r.getKind
              debugPrint(s"kind = $kind")
              for (i <- 0L until r.getNumChildren) {
                debugPrint(s"child($i) = ${r.getChild(i)}")
              }
              r
            }

            override def toString: String = {
              "satisfiable model"
            }

          }
        }
      }
    }


    override def push(): Unit =
      smt.push()

    override def pop(): Unit =
      smt.pop()


  }

  private def vecToList(v: vectorExpr): List[Expr] = {
    var i: Int = v.size().asInstanceOf[Int] - 1
    var r = List[Expr]()
    while (i >= 0) {
      r = v.get(i) :: r
      i -= 1
    }
    r
  }

  sealed private abstract class Z3ProgramType {
    def z3type: Type
  }

  private case class Z3Sort(
    z3type: Type
  ) extends Z3ProgramType

  private case class Z3UninterpretedDT(
    z3type: Type
  ) extends Z3ProgramType {
  }

  private case class Z3DataType(
    z3type: Type,
    constructors: Map[String, edu.nyu.acsys.CVC4.DatatypeConstructor]
  ) extends Z3ProgramType {

    def getConstructor(constructorName: String): edu.nyu.acsys.CVC4.DatatypeConstructor = {
      constructors.get(constructorName) match {
        case Some(c) => c
        case None =>
          throw new RuntimeException(
            s"""
               |Could not find constructor $constructorName in type ${z3type}.
               |Available constructors are: ${constructors.keys.mkString(", ")}
             """.stripMargin)
      }
    }
  }

  private case class Z3OptionType(
    dt: Type,
    none: edu.nyu.acsys.CVC4.DatatypeConstructor,
    some: edu.nyu.acsys.CVC4.DatatypeConstructor
  ) {}


  private val optionSorts: Type => Z3OptionType = Memo.mutableHashMapMemo((sort: Type) => {
    val dt = Cvc4Proxy.Datatype(s"Option_$sort")
    val none = new edu.nyu.acsys.CVC4.DatatypeConstructor(s"None_$sort")
    val some = new edu.nyu.acsys.CVC4.DatatypeConstructor(s"Some_$sort")
    some.addArg(s"Some_${sort}_value", sort)
    dt.addConstructor(none)
    dt.addConstructor(some)
    val fdt = em.mkDatatypeType(dt)
    Z3OptionType(fdt, fdt.getDatatype.get(s"None_$sort"), fdt.getDatatype.get(s"Some_$sort"))
  })


  private def makeLimitedType(name: String, limit: Option[Int]): Type =
    limit match {
      case Some(value) =>
        val t = Cvc4Proxy.Datatype(name)
        for (x <- 0 until value) {
          val c = new edu.nyu.acsys.CVC4.DatatypeConstructor(s"$name$x")
          t.addConstructor(c)
        }
        em.mkDatatypeType(t)
      case None =>
        em.mkSort(name)
    }

  private lazy val callIdSort: Type = makeLimitedType("CallId", limitCalls)
  private lazy val transactionIdSort: Type = makeLimitedType("TxId", limitTransactions)
  private lazy val invocationIdSort: Type = makeLimitedType("InvocationId", limitInvocations)

  // TODO make this a datatpye with all possible crdt operations
  //  lazy val callSort: DatatypeSort =
  //    ctxt.mkDatatypeSort("call", Array(
  //      ctxt.mkConstructor("call", "is_call", Array[String](), Array[Type](), null)
  //    ))

  private val customTypes: String => Type = Memo.mutableHashMapMemo { name: String =>
    em.mkSort(name)
  }

  private val translateDatatypeImpl: SortDatatypeImpl => Z3DataType = Memo.mutableHashMapMemo { s: SortDatatypeImpl =>
    val constructors: Map[String, edu.nyu.acsys.CVC4.DatatypeConstructor] =
      s.constructors.mapValues(c => {
        val cc = new edu.nyu.acsys.CVC4.DatatypeConstructor(c.name)
        for (a <- c.args) {
          cc.addArg(a.name, translateSort(a.typ))
        }
        cc
      }).view.force


    try {
      val t = Cvc4Proxy.Datatype(s.name)
      for (c <- constructors.values) {
        t.addConstructor(c)
      }
      val ft = em.mkDatatypeType(t)
      Z3DataType(ft, constructors.keys.map(k => k -> ft.getDatatype.get(k)).toMap)
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Could not create datatype ${s.name} with constructors $constructors", e)
    }
  }

  private val translateSortCustomUninterpreted: SortCustomUninterpreted => Type = Memo.mutableHashMapMemo { s =>
    makeLimitedType(s.name, limitCustomTypes)
  }

  private def translateSortDataType(s: SortDatatype): Z3DataType = {
    val dt = datatypeImpl(s)
    translateDatatypeImpl(dt)
  }


  private val translateSort: myMemo[SymbolicSort, Type] = new myMemo[SymbolicSort, Type]({
    case SortCustomDt(dt) =>
      translateDatatypeImpl(dt).z3type
    case s: SortCustomUninterpreted =>
      translateSortCustomUninterpreted(s)
    case s: SortDatatype =>
      translateSortDataType(s).z3type
    case SortInt() => em.integerType()
    case SortBoolean() => em.booleanType()
    case SortCallId() => callIdSort
    case SortTxId() => transactionIdSort
    //    case SortTransactionStatus() => transactionStatusSort.dt
    case SortInvocationId() =>
      invocationIdSort
    //    case SortCall() => callSort
    case SortMap(keySort, valueSort) =>
      em.mkArrayType(translateSort(keySort), translateSort(valueSort))
    case SortSet(valueSort) =>
      em.mkSetType(translateSort(valueSort))
    case SortOption(valueSort) =>
      optionSorts(translateSort(valueSort)).dt
    case SortAny() =>
      throw new RuntimeException("Cannot handle SortAny")
  })


  private def userDefinedConstructor(typ: SortDatatypeImpl, constructorName: String): edu.nyu.acsys.CVC4.DatatypeConstructor = {
    val dt = translateDatatypeImpl(typ)
    dt.getConstructor(constructorName)
  }


  //  def invocationInfoConstructor(procname: String): edu.nyu.acsys.CVC4.DatatypeConstructor =
  //    invocationInfoDatatype.constructors(procname)


  // de-bruijn-indexes of variables and so on
  case class TranslationContext(
    variableValues: Map[SymbolicVariable[_], Expr] = Map(),
    nameMap: Map[String, Expr] = Map()
  ) {

    def withVariable(v: SymbolicVariable[_ <: SymbolicSort], vt: Expr): TranslationContext = {
      if (variableValues.contains(v)) {
        throw new RuntimeException(s"variable $v already bound")
      }
      if (nameMap.contains(v.name)) {
        throw new RuntimeException(s"variable with name $v already bound")
      }
      if (globalVars.keySet().contains(v)) {
        throw new RuntimeException(s"variable $v already used as global")
      }
      this.copy(
        variableValues = variableValues + (v -> vt),
          nameMap = nameMap + (v.name -> vt)
      )
    }

  }

  def freshContext(): TranslationContext = TranslationContext()

  override def translateBool(expr: SVal[SortBoolean]): Expr = {
    translateBool(expr, freshContext())
  }

  def translateBool(expr: SVal[SortBoolean], trC: this.TranslationContext): Expr = {
    translateExprI(expr)(trC)
  }

  def translateBoolH(expr: SVal[SortBoolean])(implicit trC: this.TranslationContext): Expr = {
    translateExprI(expr)(trC).asInstanceOf[Expr]
  }

  private def translateMap[K <: SymbolicSort, V <: SymbolicSort](expr: SVal[SortMap[K, V]])(implicit trC: TranslationContext): Expr = {
    translateExprI(expr).asInstanceOf[Expr]
  }

  private def translateSet[T <: SymbolicSort](expr: SVal[SortSet[T]])(implicit trC: TranslationContext): Expr = {
    translateExprI(expr).asInstanceOf[Expr]
  }

  private def translateInt(expr: SVal[SortInt])(implicit trC: TranslationContext): Expr = {
    translateExprI(expr).asInstanceOf[Expr]
  }

  private def isTrue(expr: Expr): Expr = expr

  // optimization: The same SVal should yield the same expression because Z3 reuses this
  private val translationCache: ((SVal[_], TranslationContext)) => Expr =
    Memo.mutableHashMapMemo[(SVal[_], TranslationContext), Expr] { case (v, c) =>
      translateExprIntern(v.asInstanceOf[SVal[SymbolicSort]])(c)
    }


  override def translateExpr[T <: SymbolicSort](expr: SVal[T]): Expr = {
    translateExpr(expr, freshContext())
  }


  private def translateExpr[T <: SymbolicSort](expr: SVal[T], trC: TranslationContext): Expr = {
    translateExprI(expr)(trC)
  }

  private def translateExprI[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): Expr = {
    try {
      translationCache(expr, trC)
      //      translateExprIntern(expr.asInstanceOf[SVal[SymbolicSort]])(trC)
    } catch {
      case err: Throwable =>
        throw new RuntimeException("Error when translating\n" + expr, err)
    }
  }

  private def toVectorExpr(exprs: Iterable[Expr]): vectorExpr = {
    Cvc4Proxy.toVectorExpr(exprs)
  }

  private val globalVars: myMemo[SymbolicVariable[_ <: SymbolicSort], Expr] =
    new myMemo[SymbolicVariable[_ <: SymbolicSort], Expr](sv => {
      variables = variables + (sv.name -> translateSort(sv.typ))
      if (usedVarNames.contains(sv.name)) {
        throw new RuntimeException(s"$sv is already used...")
      }
      usedVarNames += sv.name

      em.mkVar(sv.name, translateSort(sv.typ))
    })

  private def translateExprIntern[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): Expr = expr match {
    case ConcreteVal(value) =>
      value match {
        case b: Boolean => em.mkConst(b)
        case i: BigInt => em.mkConst(new Rational(i.toString()))
        case _ =>
          throw new RuntimeException(s"unhandled concrete value $value (${value.getClass})")
      }
    case sv@SymbolicVariable(name, typ) =>
      trC.variableValues.get(sv) match {
        case Some(e) => e
        case None =>
          globalVars(sv)
      }
    case SEq(left, right) =>
      em.mkExpr(Kind.EQUAL, translateExprI(left), translateExprI(right))
    case SNotEq(left, right) =>
      em.mkExpr(Kind.NOT, em.mkExpr(Kind.EQUAL, translateExprI(left), translateExprI(right)))
    case SNone(t) =>
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, getConstructor(optionSorts(translateSort(t)).none))
    case n@SSome(value) =>
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, getConstructor(optionSorts(translateSort(n.typ.valueSort)).some), translateExprI(value))
    case s: SOptionMatch[_, _] =>
      val sort = translateSort(s.option.typ.valueSort)
      val os = optionSorts(sort)
      val option = translateExprI(s.option)
      val ifNone = translateExprI(s.ifNone)

      val ifSomeValue = em.mkExpr(Kind.APPLY_SELECTOR_TOTAL, os.some.getSelector(s"Some_${sort}_value"), option)
      val ifSome = translateExprI(s.ifSome)(trC.withVariable(s.ifSomeVariable, ifSomeValue))

      em.mkExpr(Kind.ITE,
        em.mkExpr(Kind.APPLY_TESTER, os.none.getTester, option),
        ifNone, ifSome)
    case SMapGet(map, key) =>
      em.mkExpr(Kind.SELECT, translateMap(map), translateExprI(key))
    case value: SymbolicMap[_, _] =>
      value match {
        case e@SymbolicMapEmpty(defaultValue) =>
          em.mkConst(new ArrayStoreAll(translateSort(e.typ).asInstanceOf[ArrayType], translateExprI(defaultValue)))
          em.mkExpr(Kind.ARRAY_LAMBDA, translateExprI(defaultValue))
        case SymbolicMapVar(v) =>
          translateExprI(v)
        case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
          em.mkExpr(Kind.STORE, translateMap(baseMap), translateExprI(updatedKey), translateExprI(newValue))
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetInsert(set, vals) if vals.isEmpty =>
          translateExpr(set)
        case SSetInsert(e@SSetEmpty(), vals) =>
          val lastSingle = em.mkExpr(Kind.SINGLETON, translateExpr(vals.last))
          if (vals.size == 1) {
            lastSingle
          } else {
            em.mkExpr(Kind.INSERT, toVectorExpr(vals.init.toList.map(v => translateExpr(v)) ++ List(lastSingle)))
          }
        case SSetInsert(set, vals) =>
          em.mkExpr(Kind.INSERT, toVectorExpr(vals.map(v => translateExpr(v)) ++ List(translateSet(set))))
        case e@SSetEmpty() =>
          em.mkConst(new EmptySet(translateSort(e.typ).asInstanceOf[SetType]))
        case SSetVar(v) =>
          translateExprI(v)
        case SSetUnion(a, b) =>
          em.mkExpr(Kind.UNION, translateSet(a), translateSet(b))
      }
    case SSetContains(set, v) =>
      em.mkExpr(Kind.MEMBER, translateExprI(v), translateSet(set))
    case QuantifierExpr(quantifier, variable, body) =>
      val kind = quantifier match {
        case QForall() => Kind.FORALL
        case QExists() => Kind.EXISTS
      }
      val name = variable.name
      if (usedVarNames.contains(name)) {
        throw new RuntimeException(s"$variable is already used...")
      }
      usedVarNames += name
      val v = em.mkBoundVar(name, translateSort(variable.typ))
      em.mkExpr(kind,
        em.mkExpr(Kind.BOUND_VAR_LIST, v),
        translateExprI(body)(trC.withVariable(variable, v))
      )
    case s@SCommitted() =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, getConstructor(z3t, "Committed"))
    case s@SUncommitted() =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, getConstructor(z3t, "Uncommitted"))
    case SBool(value) =>
      em.mkConst(value)
    case SNot(value) =>
      em.mkExpr(Kind.NOT, translateBool(value, trC))
    case SAnd(left, right) =>
      em.mkExpr(Kind.AND, translateBoolH(left), translateBoolH(right))
    case SOr(left, right) =>
      em.mkExpr(Kind.OR, translateBoolH(left), translateBoolH(right))
    case SImplies(left, right) =>
      em.mkExpr(Kind.IMPLIES, translateBoolH(left), translateBoolH(right))
    case SDatatypeValue(typ, constructorName, values, t) =>
      val args = toVectorExpr(values.map(v => translateExprI(v)))
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(userDefinedConstructor(typ, constructorName)),
        args)
    case fc@SFunctionCall(typ, name, args) =>
      throw new RuntimeException(s"translation missing for function call $fc")
    case s@SInvocationInfo(procname, values) =>
      val z3t = translateSortDataType(s.typ)
      val args = toVectorExpr(values.map(v => translateExprI(v)))
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, procname), args)
    case s@SInvocationInfoNone() =>
      val z3t = translateSortDataType(s.typ)
      val constructor = z3t.getConstructor("no_invocation")
      val constructorDecl = getConstructor(constructor)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, constructorDecl)
    case s@SCallInfo(c, args) =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, c), toVectorExpr(args.map(translateExprI)))
    case s: SCallInfoNone =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, "no_call"))
    case MapDomain(map) =>
      ???
    case IsSubsetOf(left, right) =>
      em.mkExpr(Kind.SUBSET, translateSet(left), translateSet(right))
    case s@SReturnVal(proc, v) =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, s"${proc}_res"), translateExprI(v))
    case s@SReturnValNone() =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, "no_return"))
    case SLessThanOrEqual(x, y) =>
      em.mkExpr(Kind.LEQ, translateInt(x), translateInt(y))
    case SLessThan(x, y) =>
      em.mkExpr(Kind.LT, translateInt(x), translateInt(y))
    case SDistinct(args) =>
      if (args.size < 2) {
        em.mkConst(true)
      } else {
        em.mkExpr(Kind.DISTINCT, toVectorExpr(args.map(translateExprI)))
      }
    case SValOpaque(k, v, t) =>
      v.asInstanceOf[Expr]
  }


  private def debugPrint(str: String): Unit = {
//    for (i <- 0 to indent)
//      print(" ")
//    println(str)
  }

  var indent = 0

  override def parseExpr[T <: SymbolicSort](expr: Expr)(implicit t: T): SVal[T] = {
    indent += 1
    val kind = expr.getKind

    lazy val children: List[Expr] =
      (for (i <- 0L until expr.getNumChildren) yield expr.getChild(i)).toList

    debugPrint(s"translate $expr")
    debugPrint(s"targetType = $t")
    debugPrint(s"kind = $kind")
//    debugPrint(s"children = $children")

    val result: SVal[_] = kind match {
      case Kind.STORE =>
        t match {
          case tt: SortMap[k, v] =>
            SymbolicMapUpdated(
              parseExpr(children(1))(tt.keySort),
              parseExpr(children(2))(tt.valueSort),
              parseExpr(children(0))(tt)).asInstanceOf[SVal[T]]

        }
      case Kind.STORE_ALL =>
        t match {
          case tt: SortMap[k, v] =>
            val const = expr.getConstArrayStoreAll
            SymbolicMapEmpty(parseExpr(const.getExpr)(tt.valueSort))(tt.keySort, tt.valueSort).asInstanceOf[SVal[T]]
        }
      case Kind.SINGLETON =>
        t match {
          case tt: SortSet[t] =>
            SSetInsert(SSetEmpty()(tt.valueSort), immutable.Set(parseExpr(children(0))(tt.valueSort))).cast
        }
      case Kind.EMPTYSET =>
        t match {
          case tt: SortSet[t] =>
            SSetEmpty()(tt.valueSort).cast
        }
      case Kind.UNION =>
        t match {
          case tt: SortSet[t] =>
            val a: SVal[SortSet[t]] = parseExpr(children(0))(tt).upcast()
            val b: SVal[SortSet[t]] = parseExpr(children(1))(tt).upcast()
            SSetUnion(a, b).cast
        }
      case Kind.APPLY_CONSTRUCTOR =>
        val constructorName = expr.getOperator.toString
        t match {
          case s: SortDatatype =>
            val dt = datatypeImpl(s)
            val constr = dt.constructors(constructorName)
            val args: List[SVal[SymbolicSort]] =
              for ((at, i) <- constr.args.zipWithIndex) yield
                parseExpr(children(i))(at.typ).cast[SymbolicSort]

            s match {
              case s: SortCall =>
                SCallInfo(constructorName, args).cast
              case s: SortInvocationRes =>
                if (args.isEmpty) {
                  SReturnVal(constructorName, SValOpaque(Kind.ABS, s"empty result $expr", SortInt())).cast
                } else {
                  SReturnVal(constructorName, args.head.asInstanceOf[SVal[SortValue]]).cast
                }
              case s: SortCustomDt =>
                SDatatypeValue(dt, constructorName, args, s).cast
              case s: SortTransactionStatus =>
                constructorName match {
                  case "Committed" => SCommitted().cast
                  case "Uncommitted" => SUncommitted().cast
                }
              case s: SortInvocationInfo =>
                SInvocationInfo(constructorName, args.asInstanceOf[List[SVal[SortValue]]]).cast
            }
          case s: SortOption[t] =>
            if (constructorName.startsWith("Some"))
              SSome(parseExpr(children(0))).cast
            else
              SNone(s).cast
        }
      case _ =>
        debugPrint(s"opaque with kind $kind")
        SValOpaque(kind, expr, t)
    }
    debugPrint(s"--> $result")

    indent -= 1
    result.asInstanceOf[SVal[T]]
  }


  /**
    * export a list of constraints to the CVC4 input language
    */
  override def exportConstraints(constraints: List[NamedConstraint]): String = {
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
        c.description -> c.translated
      }

    for (t <- translateSort.values()) {
      t match {
        case st: SortType =>
          append(s"${st.getName}: TYPE;\n")
          append(s" % cardinality = ${st.getCardinality}")
        case dt: DatatypeType =>
          val d = dt.getDatatype
          append(s"DATATYPE ${d.getName} = \n")
          val it: util.Iterator[edu.nyu.acsys.CVC4.DatatypeConstructor] = d.iterator()
          var first = true
          while (it.hasNext) {
            if (!first) {
              append(" | ")
            } else {
              append("   ")
            }
            first = false
            val c = it.next()
            append(s"${c.getName}")
            if (c.getNumArgs > 0) {
              append("(")
              var first2 = true
              for (j <- 0 until c.getNumArgs.asInstanceOf[Int]) {
                if (!first2) {
                  append(", ")
                }
                first2 = false
                val arg = c.get(j)
                append(s"${arg.getName}: ${arg.getType.getRangeType}")
              }
              append(")")
            }
            append("\n")
          }
          append("END;\n")
        case _ =>
          append("% ")
          t.toStream((i: Int) => append(i.asInstanceOf[Char]))
      }
      append("\n")
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
