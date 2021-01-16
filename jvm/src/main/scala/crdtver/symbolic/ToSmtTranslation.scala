package crdtver.symbolic

import crdtver.symbolic.smt.Smt
import crdtver.symbolic.smt.Smt.{Datatype, FuncDef, SmtExpr, SmtExprNode, Type}
import crdtver.utils.myMemo

import scala.collection.immutable.Set


/**
 * Translates Symbolic values to Smt expressions
 */
class ToSmtTranslation(
  limitInvocations: Option[Int] = None,
  limitTransactions: Option[Int] = None,
  limitCalls: Option[Int] = None,
  limitCustomTypes: Option[Int] = None,
) {


  var datatypeImpl: SortDatatype => SortDatatypeImpl = _
  private var variables: Map[String, Type] = Map()
  private var usedVarNames: Set[String] = Set()


  sealed private abstract class SmtProgramType {
    def smttype: Type
  }

  private case class SmtSort(
    smttype: Type
  ) extends SmtProgramType

  private case class SmtUninterpretedDT(
    smttype: Type
  ) extends SmtProgramType {
  }

  // TODO option could be a generic datatype
  private val optionSorts: Type => Datatype = new myMemo((sort: Type) => {
    val typeName = sort.typeName()
    Smt.Datatype(
      s"Option_${typeName}",
      List(
        Smt.DatatypeConstructor(s"None_${typeName}", List()),
        Smt.DatatypeConstructor(s"Some_${typeName}", List(Smt.Variable(s"Some_${typeName}_value", sort)))
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

  private val customTypes: String => Type = new myMemo({ name: String =>
    Smt.Sort(name)
  })

  private val translateUninterpretedFunc: UninterpretedFunction[_ <: SymbolicSort] => FuncDef = new myMemo({ f: UninterpretedFunction[_ <: SymbolicSort] =>
    FuncDef(
      f.name,
      f.args.map(translateSort),
      translateSort(f.returnType)
    )
  })

  private def translateVar(v: SymbolicVariable[_ <: SymbolicSort]): Smt.Variable =
    Smt.Variable(v.name, translateSort(v.typ))

  private val translateDatatypeImpl: SortDatatypeImpl => Smt.Datatype = new myMemo({ s: SortDatatypeImpl =>
    val constructors: List[Smt.DatatypeConstructor] =
      s.constructors.values
        .map(c => Smt.DatatypeConstructor(c.name, c.args.map(translateVar)))
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
  })

  private val translateSortCustomUninterpreted: SortCustomUninterpreted => Type = new myMemo({ s =>
    makeLimitedType(s.name, limitCustomTypes)
  })

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
    variableValues: Map[SymbolicVariable[_], SmtExpr] = Map(),
    nameMap: Map[String, SmtExpr] = Map()
  ) {

    def withVariable(v: SymbolicVariable[_ <: SymbolicSort], vt: SmtExpr): TranslationContext = {
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

  def translateBool(expr: SVal[SortBoolean]): SmtExpr = {
    translateBool(expr, freshContext())
  }

  def translateBool(expr: SVal[SortBoolean], trC: this.TranslationContext): SmtExpr = {
    translateExprI(expr)(trC)
  }

  def translateBoolH(expr: SVal[SortBoolean])(implicit trC: this.TranslationContext): SmtExpr = {
    translateExprI(expr)(trC).asInstanceOf[SmtExpr]
  }

  private def translateMap[K <: SymbolicSort, V <: SymbolicSort](expr: SVal[SortMap[K, V]])(implicit trC: TranslationContext): SmtExpr = {
    translateExprI(expr).asInstanceOf[SmtExpr]
  }

  private def translateSet[T <: SymbolicSort](expr: SVal[SortSet[T]])(implicit trC: TranslationContext): SmtExpr = {
    translateExprI(expr).asInstanceOf[SmtExpr]
  }

  private def translateInt(expr: SVal[SortInt])(implicit trC: TranslationContext): SmtExpr = {
    translateExprI(expr).asInstanceOf[SmtExpr]
  }

  private def isTrue(expr: SmtExpr): SmtExpr = expr

  // optimization: The same SVal should yield the same expression because Smt reuses this
  private val translationCache: ((SVal[_], TranslationContext)) => SmtExpr =
    new myMemo({ case (v, c) =>
      translateExprIntern(v.asInstanceOf[SVal[SymbolicSort]])(c)
    })


  def translateExpr[T <: SymbolicSort](expr: SVal[T]): SmtExpr = {
    translateExpr(expr, freshContext())
  }


  private def translateExpr[T <: SymbolicSort](expr: SVal[T], trC: TranslationContext): SmtExpr = {
    translateExprI(expr)(trC)
  }

  private def translateExprI[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): SmtExpr = {
    try {
      translationCache(expr, trC)
      //      translateExprIntern(expr.asInstanceOf[SVal[SymbolicSort]])(trC)
    } catch {
      case err: Throwable =>
        throw new RuntimeException("Error when translating\n" + expr, err)
    }
  }


  private val globalVars: myMemo[SymbolicVariable[_ <: SymbolicSort], SmtExpr] =
    new myMemo[SymbolicVariable[_ <: SymbolicSort], SmtExpr](sv => {
      variables = variables + (sv.name -> translateSort(sv.typ))
      if (usedVarNames.contains(sv.name)) {
        throw new RuntimeException(s"$sv is already used...")
      }
      usedVarNames += sv.name

      Smt.Variable(sv.name, translateSort(sv.typ))
    })


  def noneConstructor(optionType: Datatype): Smt.DatatypeConstructor =
    optionType.constructors.find(_.name.startsWith("None_")).get

  def someConstructor(optionType: Datatype): Smt.DatatypeConstructor =
    optionType.constructors.find(_.name.startsWith("Some_")).get

  private def translateExprIntern[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): SmtExpr = {
    // hide translateExpr method to avoid calling the wrong method
    def translateExpr(): Unit = ???

    expr match {
      case ConcreteVal(value) =>
        value match {
          case b: Boolean => Smt.Const(b)
          case i: BigInt => Smt.ConstI(i)
          case _ =>
            throw new RuntimeException(s"unhandled concrete value $value (${value.getClass})")
        }
      case sv@SymbolicVariable(name, _, typ) =>
        trC.variableValues.get(sv) match {
          case Some(e) => e
          case None =>
            globalVars(sv)
        }
      case SEq(left, right) =>
        Smt.Equals(translateExprI(left), translateExprI(right))
      case SNotEq(left, right) =>
        Smt.Not(Smt.Equals(translateExprI(left), translateExprI(right)))
      case SNone(t) =>
        val sort = translateSort(t)
        val optionType = optionSorts(sort)
        Smt.ApplyConstructor(optionType, noneConstructor(optionType))
      case n@SSome(value) =>
        val sort = translateSort(n.typ.valueSort)
        Smt.ApplyConstructor(optionSorts(sort), someConstructor(optionSorts(sort)), translateExprI(value))
      case s: SOptionMatch[_, _] =>
        val sort = translateSort(s.option.typ.valueSort)
        val os = optionSorts(sort)
        val option = translateExprI(s.option)
        val ifNone = translateExprI(s.ifNone)


        val ifSomeValue = Smt.ApplySelector(os, someConstructor(os), someConstructor(os).args.head, option)
        val ifSome = translateExprI(s.ifSome)(trC.withVariable(s.ifSomeVariable, ifSomeValue))

        Smt.IfThenElse(
          Smt.ApplyTester(os, noneConstructor(os), option),
          ifNone, ifSome)
      case SMapGet(map, key) =>
        Smt.MapSelect(translateMap(map), translateExprI(key))
      case e@SymbolicMapEmpty(kt, defaultValue) =>
        Smt.ConstantMap(translateSort(kt), translateExprI(defaultValue))
      case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
        Smt.MapStore(translateMap(baseMap), translateExprI(updatedKey), translateExprI(newValue))
      case SSetInsert(set, vals) =>
        Smt.SetInsert(translateExprI(set), vals.map(v => translateExprI(v)).toList)
      case e@SSetEmpty(typ) =>
        Smt.EmptySet(translateSort(typ))
      case SSetVar(v) =>
        translateExprI(v)
      case SSetUnion(a, b) =>
        Smt.Union(translateSet(a), translateSet(b))
      case SSetContains(set, v) =>
        Smt.SetContains(translateExprI(v), translateSet(set))
      case QuantifierExpr(quantifier, variable, body) =>

        val name = variable.name
        if (usedVarNames.contains(name)) {
          throw new RuntimeException(s"$variable is already used...")
        }
        usedVarNames += name
        val v = Smt.Variable(name, translateSort(variable.typ))
        val kind = quantifier match {
          case QForall() => Smt.Forall()
          case QExists() => Smt.Exists()
        }
        Smt.QuantifierExpr(kind, v, translateExprI(body)(trC.withVariable(variable, v))
        )
      case s@SCommitted() =>
        val smtt = translateSortDataType(s.typ)
        Smt.ApplyConstructor(smtt, "Committed")
      case s@SUncommitted() =>
        val smtt = translateSortDataType(s.typ)
        Smt.ApplyConstructor(smtt, "Uncommitted")
      case SBool(value) =>
        Smt.Const(value)
      case SNot(value) =>
        Smt.Not(translateBool(value, trC))
      case SAnd(left, right) =>
        Smt.And(translateBoolH(left), translateBoolH(right))
      case SOr(left, right) =>
        Smt.Or(translateBoolH(left), translateBoolH(right))
      case SImplies(left, right) =>
        Smt.Implies(translateBoolH(left), translateBoolH(right))
      case SDatatypeValue(typ, constructorName, values, t) =>
        val args = values.map(v => translateExprI(v))
        val dt = translateDatatypeImpl(typ)
        Smt.ApplyConstructor(dt, constructorName, args)
      case fc@SFunctionCall(typ, uf, values) =>
        val f: FuncDef = translateUninterpretedFunc(uf)
        val args = values.map(v => translateExprI(v))
        Smt.ApplyFunc(f, args)
      case s@SInvocationInfo(procname, values) =>
        val smtt = translateSortDataType(s.typ)
        val args = values.map(v => translateExprI(v))
        Smt.ApplyConstructor(smtt, procname, args)
      case s@SInvocationInfoNone() =>
        val smtt = translateSortDataType(s.typ)
        Smt.ApplyConstructor(smtt, "no_invocation")
      case s@SCallInfo(c, args) =>
        val smtt = translateSortDataType(s.typ)
        Smt.ApplyConstructor(smtt, c, args.map(translateExprI))
      case s: SCallInfoNone =>
        val smtt = translateSortDataType(s.typ)
        Smt.ApplyConstructor(smtt, "NoCall")
      case IsSubsetOf(s, MapDomain(m)) =>
        val v = Smt.Variable("x", translateSort(s.typ.valueSort))
        val noneValue = translateExprI(SNone(m.typ.valueSort.valueSort))
        Smt.QuantifierExpr(Smt.Forall(), v,
          Smt.Implies(
            Smt.SetContains(v, translateExprI(s)),
            Smt.Not(Smt.Equals(Smt.MapSelect(translateExprI(m), v), noneValue))))
      case MapDomain(map) =>
        ???
      case IsSubsetOf(left, right) =>
        Smt.IsSubsetOf(translateSet(left), translateSet(right))
      case s@SReturnVal(proc, v) =>
        val smtt = translateSortDataType(s.typ)
        Smt.ApplyConstructor(smtt, s"${proc}_res", translateExprI(v))
      case s@SReturnValNone() =>
        val smtt = translateSortDataType(s.typ)
        Smt.ApplyConstructor(smtt, "NoResult")
      case SLessThanOrEqual(x, y) =>
        Smt.Leq(translateInt(x), translateInt(y))
      case SLessThan(x, y) =>
        Smt.Lt(translateInt(x), translateInt(y))
      case SDistinct(args) =>
        if (args.size < 2) {
          Smt.Const(true)
        } else {
          Smt.Distinct(args.map(translateExprI))
        }
      case SValOpaque(v, t) =>
        Smt.OpaqueExpr(translateSort(t), v)
      case SNamedVal(_, v) =>
        translateExprIntern(v)
      case s@SChooseSome(condition, variable) =>
        throw new RuntimeException("Cannot translate SChooseSome " + s.prettyPrint)
      case s@SAggregateExpr(op, variables, filter, elem) =>

        /**
         * TODO
         * translate to a new variable.
         * The idea is that equivalent expressions should be assigned the same variable.
         * And the simplifier can convert conjunctions, disjunctions, set inserts, empty set into if expressions?
         */
        throw new RuntimeException("Cannot translate SAggregateExpr " + s.prettyPrint)
      case SBinaryInt(op, left, right) =>
        val smtOp: (SmtExpr, SmtExpr) => SmtExprNode =
          op match {
            case SPlus() => Smt.Plus
            case SMinus() => Smt.Minus
            case SMult() => Smt.Mult
            case SDiv() => Smt.Div
            case SMod() => Smt.Mod
          }
        smtOp(translateExprI(left), translateExprI(right))

    }
  }

  def parseExpr[T <: SymbolicSort](expr: SmtExpr, t: T): SVal[T] = {
    expr match {
      case node: Smt.SmtExprNode =>
        node match {
          case Smt.Equals(left, right) =>
            ???
          case Smt.Not(of) =>
            ???
          case Smt.Div(_, _) => ???
          case Smt.Minus(_, _) => ???
          case Smt.Mod(_, _) => ???
          case Smt.Mult(_, _) => ???
          case Smt.Plus(_, _) =>
            ???
          case ac: Smt.ApplyConstructor =>
            val constructorName = ac.constructor.name
            t match {
              case s: SortDatatype =>
                val dt = datatypeImpl(s)
                val constr = dt.constructors(constructorName)
                val args: List[SVal[SymbolicSort]] =
                  for ((p, a) <- constr.args.zip(ac.args)) yield
                    parseExpr[SymbolicSort](a, p.typ)

                s match {
                  case s: SortCall =>
                    SCallInfo(constructorName, args).cast(t)
                  case s: SortInvocationRes =>
                    if (constructorName == "NoResult") {
                      SReturnValNone().cast(t)
                    } else if (args.isEmpty) {
                      SReturnVal(constructorName, SValOpaque(s"empty result $expr", SortInt())).cast(t)
                    } else {
                      SReturnVal(constructorName, args.head.asInstanceOf[SVal[SortValue]]).cast(t)
                    }
                  case s: SortCustomDt =>
                    SDatatypeValue(dt, constructorName, args, s).cast(t)
                  case s: SortTransactionStatus =>
                    constructorName match {
                      case "Committed" => SCommitted().cast(t)
                      case "Uncommitted" => SUncommitted().cast(t)
                    }
                  case s: SortInvocationInfo =>
                    SInvocationInfo(constructorName, args.asInstanceOf[List[SVal[SortValue]]]).cast(t)
                }
              case s: SortOption[t] =>
                if (constructorName.startsWith("Some"))
                  SSome(parseExpr(ac.args(0), s.valueSort)).cast(t)
                else
                  SNone(s.valueSort).cast(t)
              case tt =>
                throw new RuntimeException(s"unhandled case $ac: $tt (${tt.getClass})")
            }
          case Smt.ApplySelector(dt, constructor, variable, expr) =>
            ???
          case Smt.IfThenElse(cond, ifTrue, ifFalse) =>
            ???
          case Smt.ApplyTester(dt, constructor, expr) =>
            ???
          case Smt.MapSelect(map, key) =>
            ???
          case Smt.ConstantMap(keyType, defaultValue) =>
            t match {
              case tt: SortMap[k, v] =>
                SymbolicMapEmpty(tt.keySort, parseExpr(defaultValue, tt.valueSort))(tt.valueSort).cast(t)
              case tt: SortSet[k] =>
                require(defaultValue == Smt.Const(false))
                SSetEmpty(tt).cast(t)
            }
          case Smt.MapStore(map, key, newValue) =>
            t match {
              case tm: SortMap[k, v] =>
                SymbolicMapUpdated(parseExpr(key, tm.keySort), parseExpr(newValue, tm.valueSort), parseExpr[SortMap[k, v]](map, tm)).cast(t)
            }
          case Smt.SetSingleton(value) =>
            t match {
              case tt: SortSet[t] =>
                SSetInsert(SSetEmpty(tt.valueSort), Set(parseExpr[t](value, tt.valueSort))).cast(t)
            }
          case Smt.SetInsert(set, values) =>
            t match {
              case tt: SortSet[t] =>
                val pSet: SVal[SortSet[t]] = parseExpr(set, tt)
                val pValues = values.map(v => parseExpr(v, tt.valueSort))
                SSetInsert(pSet, pValues.toSet).cast(t)
            }
          case Smt.Union(left, right) =>
            t match {
              case tt: SortSet[t] =>
                val a: SVal[SortSet[t]] = parseExpr(left, tt).upcast
                val b: SVal[SortSet[t]] = parseExpr(right, tt).upcast
                SSetUnion(a, b).cast(t)
            }
          case Smt.QuantifierExpr(quantifier, variable, expr) =>
            ???
          case Smt.And(left, right) =>
            ???
          case Smt.Or(left, right) =>
            ???
          case Smt.Implies(left, right) =>
            ???
          case Smt.IsSubsetOf(left, right) =>
            ???
          case Smt.SetContains(elem, set) =>
            ???
          case Smt.Leq(left, right) =>
            ???
          case Smt.Lt(left, right) =>
            ???
          case Smt.ApplyFunc(f, args) =>
            ???
          case Smt.Distinct(elems) =>
            ???
        }
      case Smt.Variable(name, typ) =>
        SymbolicVariable(name, false, t)
      case Smt.Const(b) =>
        ConcreteVal(b)(t)
      case Smt.ConstI(i) =>
        ConcreteVal(i)(t)
      case Smt.EmptySet(valueType) =>
        t match {
          case tt: SortSet[t] =>
            SSetEmpty(tt.valueSort).cast(t)
        }
      case Smt.OpaqueExpr(kind, e) =>
        SValOpaque(e, t)
    }
  }

}
