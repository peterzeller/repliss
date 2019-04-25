package crdtver.symbolic

import crdtver.symbolic.smt.Smt
import crdtver.symbolic.smt.Smt.{Datatype, SmtExpr, Type}
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
) {


  var datatypeImpl: SortDatatype => SortDatatypeImpl = _
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

  // optimization: The same SVal should yield the same expression because Z3 reuses this
  private val translationCache: ((SVal[_], TranslationContext)) => SmtExpr =
    Memo.mutableHashMapMemo[(SVal[_], TranslationContext), SmtExpr] { case (v, c) =>
      translateExprIntern(v.asInstanceOf[SVal[SymbolicSort]])(c)
    }


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

  private def translateExprIntern[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): SmtExpr = expr match {
    case ConcreteVal(value) =>
      value match {
        case b: Boolean => Smt.Const(b)
        case i: BigInt => Smt.ConstI(i)
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
    case value: SymbolicMap[_, _] =>
      value match {
        case e@SymbolicMapEmpty(defaultValue) =>
          Smt.ConstantMap(translateSort(value.typ.keySort), translateExprI(defaultValue))
        case SymbolicMapVar(v) =>
          translateExprI(v)
        case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
          Smt.MapStore(translateMap(baseMap), translateExprI(updatedKey), translateExprI(newValue))
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetInsert(set, vals) =>
          Smt.SetInsert(translateExpr(set), vals.map(v => translateExpr(v)).toList)
        case e@SSetEmpty() =>
          Smt.EmptySet(translateSort(e.typ))
        case SSetVar(v) =>
          translateExprI(v)
        case SSetUnion(a, b) =>
          Smt.Union(translateSet(a), translateSet(b))
      }
    case SSetContains(set, v) =>
      Smt.Member(translateExprI(v), translateSet(set))
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
      val z3t = translateSortDataType(s.typ)
      Smt.ApplyConstructor(z3t, "Committed")
    case s@SUncommitted() =>
      val z3t = translateSortDataType(s.typ)
      Smt.ApplyConstructor(z3t, "Uncommitted")
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
    case fc@SFunctionCall(typ, name, args) =>
      throw new RuntimeException(s"translation missing for function call $fc")
    case s@SInvocationInfo(procname, values) =>
      val z3t = translateSortDataType(s.typ)
      val args = values.map(v => translateExprI(v))
      Smt.ApplyConstructor(z3t, procname, args)
    case s@SInvocationInfoNone() =>
      val z3t = translateSortDataType(s.typ)
      Smt.ApplyConstructor(z3t, "no_invocation")
    case s@SCallInfo(c, args) =>
      val z3t = translateSortDataType(s.typ)
      Smt.ApplyConstructor(z3t, c, args.map(translateExprI))
    case s: SCallInfoNone =>
      val z3t = translateSortDataType(s.typ)
      Smt.ApplyConstructor(z3t, "no_call")
    case IsSubsetOf(s, MapDomain(m)) =>
      val v = Smt.Variable("x", translateSort(s.typ.valueSort))
      val noneValue = translateExpr(SNone(m.typ.valueSort.valueSort))
      Smt.QuantifierExpr(Smt.Forall(), v,
        Smt.Implies(
          Smt.SetContains(v, translateExpr(s)),
          Smt.Not(Smt.Equals(Smt.MapSelect(translateExpr(m), v), noneValue))))
    case MapDomain(map) =>
      ???
    case IsSubsetOf(left, right) =>
      Smt.IsSubsetOf(translateSet(left), translateSet(right))
    case s@SReturnVal(proc, v) =>
      val z3t = translateSortDataType(s.typ)
      Smt.ApplyConstructor(z3t, s"${proc}_res", translateExprI(v))
    case s@SReturnValNone() =>
      val z3t = translateSortDataType(s.typ)
      Smt.ApplyConstructor(z3t, "no_return")
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
    case SValOpaque(k, v, t) =>
      Smt.OpaqueExpr(k, v)
  }
  def parseExpr[T <: SymbolicSort](expr: SmtExpr, t: T): SVal[T] = {
    println(s"parseExpr($expr, $t)")
    expr match {
      case node: Smt.SmtExprNode =>
        node match {
          case Smt.Equals(left, right) =>
            ???
          case Smt.Not(of) =>
            ???
          case ac: Smt.ApplyConstructor =>
            val constructorName = ac.constructor.name
            t match {
              case s: SortDatatype =>
                val dt = datatypeImpl(s)
                val constr = dt.constructors(constructorName)
                val args: List[SVal[SymbolicSort]] =
                  for ((p,a) <- constr.args.zip(ac.args)) yield
                    parseExpr[SymbolicSort](a, p.typ)

                s match {
                  case s: SortCall =>
                    SCallInfo(constructorName, args).cast
                  case s: SortInvocationRes =>
                    if (args.isEmpty) {
                      SReturnVal(constructorName, SValOpaque("", s"empty result $expr", SortInt())).cast
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
                  SSome(parseExpr(ac.args(0), s.valueSort)).cast
                else
                  SNone(s).cast
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
                SymbolicMapEmpty(parseExpr(defaultValue, tt.valueSort))(tt.keySort, tt.valueSort).cast
            }
          case Smt.MapStore(map, key, newValue) =>
            t match {
              case tm: SortMap[k,v] =>
                SymbolicMapUpdated(parseExpr(key, tm.keySort), parseExpr(newValue, tm.valueSort), parseExpr(map, tm)).cast
            }
          case Smt.SetSingleton(value) =>
            t match {
              case tt: SortSet[t] =>
                SSetInsert(SSetEmpty()(tt.valueSort), immutable.Set(parseExpr(value, tt.valueSort))).cast
            }
          case Smt.SetInsert(set, values) =>
            ???
          case Smt.Union(left, right) =>
            t match {
              case tt: SortSet[t] =>
                val a: SVal[SortSet[t]] = parseExpr(left, tt).upcast()
                val b: SVal[SortSet[t]] = parseExpr(right, tt).upcast()
                SSetUnion(a, b).cast
            }
          case Smt.Member(value, set) =>
            ???
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
        }
      case Smt.Variable(name, typ) =>
        ???
      case Smt.Const(b) =>
        ???
      case Smt.ConstI(i) =>
        ???
      case Smt.EmptySet(valueType) =>
        t match {
          case tt: SortSet[t] =>
            SSetEmpty()(tt.valueSort).cast
        }
      case Smt.Distinct(elems) =>
        ???
      case Smt.OpaqueExpr(kind, e) =>
        SValOpaque(kind, e, t)
    }
  }

}
