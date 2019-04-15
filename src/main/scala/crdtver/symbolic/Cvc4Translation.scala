package crdtver.symbolic

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util

import crdtver.symbolic.Cvc4Proxy.getConstructor
import crdtver.symbolic.smt.Smt
import crdtver.symbolic.smt.Smt.{Datatype, Type}
import crdtver.utils.myMemo
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

  private var variables: Map[String, Type] = Map()
  private var usedVarNames: Set[String] = Set()



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


  private val optionSorts: Type => Datatype = Memo.mutableHashMapMemo((sort: Type) => {
    Smt.Datatype(
      s"Option_$sort",
      List(
        Smt.DatatypeConstructor(s"None_$sort", List()),
        Smt.DatatypeConstructor(s"Some_$sort", List(Smt.Variable(s"Some_${sort}_value", sort)))
      )
    )
  })


  private def makeLimitedType(name: String, limit: Option[Int]): Type =
    limit match {
      case Some(value) =>
        val t = Cvc4Proxy.Datatype(name)
        val constructors = for (x <- 0 until value) yield {
          Smt.DatatypeConstructor(s"$name$x", List())
        }
        Smt.Datatype(name, constructors.toList)
      case None =>
        Smt.Sort(name)
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
    Smt.Sort(name)
  }

  private val translateDatatypeImpl: SortDatatypeImpl => Smt.Datatype = Memo.mutableHashMapMemo { s: SortDatatypeImpl =>
    val constructors: List[Smt.DatatypeConstructor] =
      s.constructors.values.map(c => {
        val cc = Smt.DatatypeConstructor(c.name,
          c.args.map(a => Smt.Variable(a.name, translateSort(a.typ))))
        cc
      })
      .toList


    try {
      Smt.Datatype(
        s.name,
        constructors
      )
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Could not create datatype ${s.name} with constructors $constructors", e)
    }
  }

  private val translateSortCustomUninterpreted: SortCustomUninterpreted => Type = Memo.mutableHashMapMemo { s =>
    makeLimitedType(s.name, limitCustomTypes)
  }

  private def translateSortDataType(s: SortDatatype): Datatype = {
    val dt = datatypeImpl(s)
    translateDatatypeImpl(dt)
  }


  private val translateSort: myMemo[SymbolicSort, Type] = new myMemo[SymbolicSort, Type]({
    case SortCustomDt(dt) =>
      translateDatatypeImpl(dt)
    case s: SortCustomUninterpreted =>
      translateSortCustomUninterpreted(s)
    case s: SortDatatype =>
      translateSortDataType(s)
    case SortInt() => Smt.IntegerType()
    case SortBoolean() => Smt.BoolType()
    case SortCallId() => callIdSort
    case SortTxId() => transactionIdSort
    //    case SortTransactionStatus() => transactionStatusSort.dt
    case SortInvocationId() =>
      invocationIdSort
    //    case SortCall() => callSort
    case SortMap(keySort, valueSort) =>
      Smt.ArrayType(translateSort(keySort), translateSort(valueSort))
    case SortSet(valueSort) =>
      Smt.SetType(translateSort(valueSort))
    case SortOption(valueSort) =>
      optionSorts(translateSort(valueSort))
    case SortAny() =>
      throw new RuntimeException("Cannot handle SortAny")
  })


  private def userDefinedConstructor(typ: SortDatatypeImpl, constructorName: String): Smt.DatatypeConstructor = {
    val dt = translateDatatypeImpl(typ)
    dt.getConstructor(constructorName)
  }


  //  def invocationInfoConstructor(procname: String): Smt.DatatypeConstructor =
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

      Smt.Var(sv.name, translateSort(sv.typ))
    })

  private def translateExprIntern[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): Expr = expr match {
    case ConcreteVal(value) =>
      value match {
        case b: Boolean => Smt.Const(b)
        case i: BigInt => Smt.Const(new Rational(i.toString()))
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
      Smt.Expr(Kind.EQUAL, translateExprI(left), translateExprI(right))
    case SNotEq(left, right) =>
      Smt.Expr(Kind.NOT, Smt.Expr(Kind.EQUAL, translateExprI(left), translateExprI(right)))
    case SNone(t) =>
      Smt.Expr(Kind.APPLY_CONSTRUCTOR, getConstructor(optionSorts(translateSort(t)).none))
    case n@SSome(value) =>
      Smt.Expr(Kind.APPLY_CONSTRUCTOR, getConstructor(optionSorts(translateSort(n.typ.valueSort)).some), translateExprI(value))
    case s: SOptionMatch[_, _] =>
      val sort = translateSort(s.option.typ.valueSort)
      val os = optionSorts(sort)
      val option = translateExprI(s.option)
      val ifNone = translateExprI(s.ifNone)

      val ifSomeValue = Smt.Expr(Kind.APPLY_SELECTOR_TOTAL, Cvc4Proxy.getSelector(os.some, s"Some_${sort}_value"), option)
      val ifSome = translateExprI(s.ifSome)(trC.withVariable(s.ifSomeVariable, ifSomeValue))

      Smt.Expr(Kind.ITE,
        Smt.Expr(Kind.APPLY_TESTER, Cvc4Proxy.getTester(os.none), option),
        ifNone, ifSome)
    case SMapGet(map, key) =>
      Smt.Expr(Kind.SELECT, translateMap(map), translateExprI(key))
    case value: SymbolicMap[_, _] =>
      value match {
        case e@SymbolicMapEmpty(defaultValue) =>
          Smt.Const(new ArrayStoreAll(translateSort(e.typ).asInstanceOf[ArrayType], translateExprI(defaultValue)))
          Smt.Expr(Kind.ARRAY_LAMBDA, translateExprI(defaultValue))
        case SymbolicMapVar(v) =>
          translateExprI(v)
        case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
          Smt.Expr(Kind.STORE, translateMap(baseMap), translateExprI(updatedKey), translateExprI(newValue))
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetInsert(set, vals) if vals.isEmpty =>
          translateExpr(set)
        case SSetInsert(e@SSetEmpty(), vals) =>
          val lastSingle = Smt.Expr(Kind.SINGLETON, translateExpr(vals.last))
          if (vals.size == 1) {
            lastSingle
          } else {
            Smt.Expr(Kind.INSERT, toVectorExpr(vals.init.toList.map(v => translateExpr(v)) ++ List(lastSingle)))
          }
        case SSetInsert(set, vals) =>
          Smt.Expr(Kind.INSERT, toVectorExpr(vals.map(v => translateExpr(v)) ++ List(translateSet(set))))
        case e@SSetEmpty() =>
          Smt.Const(Cvc4Proxy.mkEmptySet(translateSort(e.typ).asInstanceOf[SetType]))
        case SSetVar(v) =>
          translateExprI(v)
        case SSetUnion(a, b) =>
          Smt.Expr(Kind.UNION, translateSet(a), translateSet(b))
      }
    case SSetContains(set, v) =>
      Smt.Expr(Kind.MEMBER, translateExprI(v), translateSet(set))
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
      val v = Smt.BoundVar(name, translateSort(variable.typ))
      Smt.Expr(kind,
        Smt.Expr(Kind.BOUND_VAR_LIST, v),
        translateExprI(body)(trC.withVariable(variable, v))
      )
    case s@SCommitted() =>
      val z3t = translateSortDataType(s.typ)
      Smt.Expr(Kind.APPLY_CONSTRUCTOR, getConstructor(z3t, "Committed"))
    case s@SUncommitted() =>
      val z3t = translateSortDataType(s.typ)
      Smt.Expr(Kind.APPLY_CONSTRUCTOR, getConstructor(z3t, "Uncommitted"))
    case SBool(value) =>
      Smt.Const(value)
    case SNot(value) =>
      Smt.Expr(Kind.NOT, translateBool(value, trC))
    case SAnd(left, right) =>
      Smt.Expr(Kind.AND, translateBoolH(left), translateBoolH(right))
    case SOr(left, right) =>
      Smt.Expr(Kind.OR, translateBoolH(left), translateBoolH(right))
    case SImplies(left, right) =>
      Smt.Expr(Kind.IMPLIES, translateBoolH(left), translateBoolH(right))
    case SDatatypeValue(typ, constructorName, values, t) =>
      val args = toVectorExpr(values.map(v => translateExprI(v)))
      Smt.Expr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(userDefinedConstructor(typ, constructorName)),
        args)
    case fc@SFunctionCall(typ, name, args) =>
      throw new RuntimeException(s"translation missing for function call $fc")
    case s@SInvocationInfo(procname, values) =>
      val z3t = translateSortDataType(s.typ)
      val args = toVectorExpr(values.map(v => translateExprI(v)))
      Smt.Expr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, procname), args)
    case s@SInvocationInfoNone() =>
      val z3t = translateSortDataType(s.typ)
      val constructor = z3t.getConstructor("no_invocation")
      val constructorDecl = getConstructor(constructor)
      Smt.Expr(Kind.APPLY_CONSTRUCTOR, constructorDecl)
    case s@SCallInfo(c, args) =>
      val z3t = translateSortDataType(s.typ)
      Smt.Expr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, c), toVectorExpr(args.map(translateExprI)))
    case s: SCallInfoNone =>
      val z3t = translateSortDataType(s.typ)
      Smt.Expr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, "no_call"))
    case MapDomain(map) =>
      ???
    case IsSubsetOf(left, right) =>
      Smt.Expr(Kind.SUBSET, translateSet(left), translateSet(right))
    case s@SReturnVal(proc, v) =>
      val z3t = translateSortDataType(s.typ)
      Smt.Expr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, s"${proc}_res"), translateExprI(v))
    case s@SReturnValNone() =>
      val z3t = translateSortDataType(s.typ)
      Smt.Expr(Kind.APPLY_CONSTRUCTOR,
        getConstructor(z3t, "no_return"))
    case SLessThanOrEqual(x, y) =>
      Smt.Expr(Kind.LEQ, translateInt(x), translateInt(y))
    case SLessThan(x, y) =>
      Smt.Expr(Kind.LT, translateInt(x), translateInt(y))
    case SDistinct(args) =>
      if (args.size < 2) {
        Smt.Const(true)
      } else {
        Smt.Expr(Kind.DISTINCT, toVectorExpr(args.map(translateExprI)))
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
          val d = Cvc4Proxy.getDatatype(dt)
          append(s"DATATYPE ${d.getName} = \n")
          val it: util.Iterator[Smt.DatatypeConstructor] = d.iterator()
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
