package crdtver.symbolic

import java.io.OutputStream
import java.util

import edu.nyu.acsys.CVC4
import edu.nyu.acsys.CVC4._
import scalaz.Memo

import scala.collection.{Set, mutable}


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
  override type TTranslationContext = this.TranslationContext

  private var variables: Map[String, Type] = Map()
  private var usedVarNames: Set[String] = Set()

  System.loadLibrary("cvc4jni")

  val em = new ExprManager()


  def mkSolver(): SmtSolver = new SmtSolver() {

    val smt = new SmtEngine(em)
    smt.setOption("produce-models", new SExpr(true))
    smt.setOption("finite-model-find", new SExpr(true))
    smt.setOption("e-matching", new SExpr(true))
    smt.setOption("incremental", new SExpr(true))
    smt.setOption("tlimit", new SExpr(30000))
    smt.setOption("produce-assertions", new SExpr(true))
    smt.setOption("output-language", new SExpr("cvc4")); // Set the output-language to CVC's
    smt.setOption("default-dag-thresh", new SExpr(0)); //Disable dagifying the output


    override def add(translated: Expr): Unit = {
      smt.assertFormula(translated)
      println(s"Assert formula:\n$translated")
    }

    override def check(): CheckRes = {
      val res = smt.checkSat()
      println(s"SAT result = $res")
      val sat: Result.Sat = res.isSat
      if (sat == Result.Sat.UNSAT) {
        Unsatisfiable()
      } else if (sat == Result.Sat.SAT_UNKNOWN) {
        println(s"unknown because ${res.whyUnknown()}")
        Unknown()
      } else {
        new Satisfiable {

          override def getModel: Model = new Model {
            override def eval(expr: Expr, bool: Boolean): Expr =
              smt.getValue(expr)

            override def toString: String = {
              "satisfiable model"
            }

            private def printCsv4(): String = {
              val r = new StringBuilder()

              def append(o: Any): Unit = {
                r.append(o)
              }

              append("CVC4 input:\n\n")
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

              for (a <- vecToList(smt.getAssertions)) {
                //                append(s";AST : ")
                //                a.printAst(i => append(i.asInstanceOf[Char]))
                append("\n")
                append(s"ASSERT $a;\n")
              }

              //              for ((t,s) <- translateSort) {
              //                r.append("\n\n")
              //                r.append(s"Type $t --> $s\n")
              //                try {
              //                  val univ = smt.getValue(em.mkNullaryOperator(em.mkSetType(s), Kind.UNIVERSE_SET))
              //                  r.append(s"Univ = $univ\n${univ.getClass}")
              //                } catch {
              //                  case t: Throwable =>
              //                    r.append(s"Could not get univ: $t")
              //                }
              //              }

              append("CHECKSAT TRUE;")
              append("\n\n")
              //              smt.printInstantiations(new OutputStream() {
              //                override def write(i: Int): Unit = {
              //                  if (i >= 0) {
              //                    append(i.asInstanceOf[Char])
              //                  }
              //                }
              //              })
              r.toString()
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
    val dt = new Datatype(s"Option_$sort")
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
        val t = new Datatype(name)
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
      val t = new Datatype(s.name)
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

  private class myMemo[K, V](f: K => V) extends (K => V) with Iterable[(K, V)] {
    private val cache = new mutable.LinkedHashMap[K, V]()

    override def apply(key: K): V = {
      cache.getOrElseUpdate(key, f(key))
    }

    def keySet(): Set[K] = cache.keySet

    def values(): Iterable[V] = cache.values

    override def iterator: Iterator[(K, V)] = cache.iterator
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
  })


  private def userDefinedConstructor(typ: SortDatatypeImpl, constructorName: String): edu.nyu.acsys.CVC4.DatatypeConstructor = {
    val dt = translateDatatypeImpl(typ)
    dt.getConstructor(constructorName)
  }


  //  def invocationInfoConstructor(procname: String): edu.nyu.acsys.CVC4.DatatypeConstructor =
  //    invocationInfoDatatype.constructors(procname)


  // de-bruijn-indexes of variables and so on
  case class TranslationContext(
    indexes: Map[SymbolicVariable[_], Expr] = Map(),
    variableValues: Map[SymbolicVariable[_], Expr] = Map()
  ) {
    def withSpecialVariable(v: SymbolicVariable[_ <: SymbolicSort], value: Expr): TranslationContext =
      this.copy(
        variableValues = variableValues + (v -> value)
      )

    def pushVariable(v: SymbolicVariable[_ <: SymbolicSort], vt: Expr): TranslationContext = {
      this.copy(
        indexes = indexes + (v -> vt)
      )
    }

  }

  def freshContext(): TranslationContext = TranslationContext()

  override def translateBool(expr: SVal[SortBoolean], trC: this.TranslationContext): Expr = {
    translateExprI(expr)(trC).asInstanceOf[Expr]
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

  override def translateExpr[T <: SymbolicSort](expr: SVal[T], trC: TranslationContext): Expr = {
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
    val r = new vectorExpr()
    for (e <- exprs)
      r.add(e)
    r
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
          trC.indexes.get(sv) match {
            case Some(i) =>
              i
            case None =>
              globalVars(sv)
          }
      }
    case SEq(left, right) =>
      em.mkExpr(Kind.EQUAL, translateExprI(left), translateExprI(right))
    case SNotEq(left, right) =>
      em.mkExpr(Kind.NOT, em.mkExpr(Kind.EQUAL, translateExprI(left), translateExprI(right)))
    case SNone(t) =>
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, optionSorts(translateSort(t)).none.getConstructor)
    case n@SSome(value) =>
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, optionSorts(translateSort(n.typ.valueSort)).some.getConstructor, translateExprI(value))
    case s: SOptionMatch[_, _] =>
      val os = optionSorts(translateSort(s.option.typ.valueSort))
      val option = translateExprI(s.option)
      val ifNone = translateExprI(s.ifNone)

      val ifSomeValue = em.mkExpr(Kind.APPLY_SELECTOR_TOTAL, os.some.getSelector("value"))
      val ifSome = translateExprI(s.ifSome)(trC.withSpecialVariable(s.ifSomeVariable, ifSomeValue))

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
        case m: SymbolicMapUpdatedConcrete[_, _, _] =>
          translateExprI(m.toSymbolicUpdates())
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetInsert(set, v) =>
          em.mkExpr(Kind.INSERT, translateExprI(v), translateSet(set))
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
      val universal = quantifier == QForall()
      val kind = quantifier match {
        case QForall() => Kind.FORALL
        case QExists() => Kind.EXISTS
      }
      if (usedVarNames.contains(variable.name)) {
        throw new RuntimeException(s"$variable is already used...")
      }
      usedVarNames += variable.name
      val v = em.mkBoundVar(variable.name, translateSort(variable.typ))
      em.mkExpr(kind,
        em.mkExpr(Kind.BOUND_VAR_LIST, v),
        translateExprI(body)(trC.pushVariable(variable, v))
      )
    case s@SCommitted() =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, z3t.getConstructor("Committed").getConstructor)
    case s@SUncommitted() =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, z3t.getConstructor("Uncommitted").getConstructor)
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
      val args = new vectorExpr()
      for (v <- values) {
        args.add(translateExprI(v))
      }
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        userDefinedConstructor(typ, constructorName).getConstructor,
        args)
    case fc@SFunctionCall(typ, name, args) =>
      throw new RuntimeException(s"translation missing for function call $fc")
    case s@SInvocationInfo(procname, values) =>
      val z3t = translateSortDataType(s.typ)
      val args = new vectorExpr()
      for (v <- values) {
        args.add(translateExprI(v))
      }
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        z3t.getConstructor(procname).getConstructor, args)
    case s@SInvocationInfoNone() =>
      val z3t = translateSortDataType(s.typ)
      val constructor = z3t.getConstructor("no_invocation")
      val constructorDecl = constructor.getConstructor
      em.mkExpr(Kind.APPLY_CONSTRUCTOR, constructorDecl)
    case s@SCallInfo(c, args) =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        z3t.getConstructor(c).getConstructor, toVectorExpr(args.map(translateExprI)))
    case s: SCallInfoNone =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        z3t.getConstructor("no_call").getConstructor())
    case MapDomain(map) =>
      ???
    case IsSubsetOf(left, right) =>
      em.mkExpr(Kind.SUBSET, translateSet(left), translateSet(right))
    case s@SReturnVal(proc, v) =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        z3t.getConstructor(s"${proc}_res").getConstructor, translateExprI(v))
    case s@SReturnValNone() =>
      val z3t = translateSortDataType(s.typ)
      em.mkExpr(Kind.APPLY_CONSTRUCTOR,
        z3t.getConstructor("no_return").getConstructor)
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
  }


}
