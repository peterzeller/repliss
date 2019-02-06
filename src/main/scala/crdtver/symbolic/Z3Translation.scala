package crdtver.symbolic

import com.microsoft.z3._
import crdtver.language.InputAst
import crdtver.language.InputAst.{InProgram, InTypeExpr}


/**
  *
  */
class Z3Translation(
  val prog: InProgram,
) {
  val ctxt = new Context()
  var symbolicContext: SymbolicContext = _

  createDatatypes()


  def createDatatypes(): Unit = {
    // TODO create datatypes from the program
  }


  private case class Z3OptionType(
    dt: DatatypeSort,
    none: Constructor,
    some: Constructor
  ) {}

  private val optionSorts: Map[Sort, Z3OptionType] = Map[Sort, Z3OptionType]().withDefault((sort: Sort) => {
    val none = ctxt.mkConstructor(s"None_$sort", s"is_none_$sort", Array[String](), Array[Sort](), null)
    val some = ctxt.mkConstructor(s"Some_$sort", s"is_some_$sort", Array("value"), Array(sort), null)
    val dt = ctxt.mkDatatypeSort(s"Option_$sort", Array(none, some))
    Z3OptionType(dt, none, some)
  })

  private case class TransactionStatusSort(dt: DatatypeSort, committed: Constructor, uncommitted: Constructor)

  private lazy val transactionStatusSort: TransactionStatusSort = {
    val committed = ctxt.mkConstructor("Committed", "is_committed", Array[String](), Array[Sort](), null)
    val uncommitted = ctxt.mkConstructor("Uncommitted", "is_uncommitted", Array[String](), Array[Sort](), null)
    val dt = ctxt.mkDatatypeSort("TransactionStatus", Array(committed, uncommitted))
    TransactionStatusSort(dt, committed, uncommitted)
  }

  private case class ReturnDatatype(dt: DatatypeSort, constructors: Map[String, Constructor])

  private lazy val returnDatatype: ReturnDatatype = {
    val constructors: Map[String, Constructor] =
      (for (p <- prog.procedures) yield {
        p.returnType match {
          case Some(rt) =>
            val rt2 = ExprTranslation.translateType(rt)(symbolicContext)
            p.name.name -> ctxt.mkConstructor(s"${p.name.name}_return", s"is_${p.name.name}_return", Array("value"), Array(translateSort(rt2)), null)
          case None =>
            p.name.name -> ctxt.mkConstructor(s"${p.name.name}_return", s"is_${p.name.name}_return", Array[String](), Array[Sort](), null)
        }

      }).toMap


    ReturnDatatype(
      ctxt.mkDatatypeSort("ReturnValue", constructors.values.toArray),
      constructors
    )
  }

  private case class InvocationInfoDatatype(dt: DatatypeSort, constructors: Map[String, Constructor])

  private lazy val invocationInfoDatatype: InvocationInfoDatatype = {
    val constructors: Map[String, Constructor] =
      (for (p <- prog.procedures) yield {
        val paramNames = p.params.map(_.name.name).toArray
        val paramSorts = p.params.map(param => translateSort(ExprTranslation.translateType(param.typ)(symbolicContext))).toArray
        p.name.name -> ctxt.mkConstructor(s"${p.name.name}_invoc", s"is_${p.name.name}_invoc", paramNames, paramSorts, null)
      }).toMap


    InvocationInfoDatatype(
      ctxt.mkDatatypeSort("InvocationInfo", constructors.values.toArray),
      constructors
    )
  }

  lazy val callIdSort: Sort = ctxt.mkUninterpretedSort("callId")
  lazy val transactionIdSort: Sort = ctxt.mkUninterpretedSort("txId")
  lazy val invocationIdSort: Sort = ctxt.mkUninterpretedSort("invocationId")
  lazy val uidSort: Sort = ctxt.mkUninterpretedSort("uid")

  // TODO make this a datatpye with all possible crdt operations
  lazy val callSort: DatatypeSort =
    ctxt.mkDatatypeSort("call", Array(
      ctxt.mkConstructor("call", "is_call", Array[String](), Array[Sort](), null)
    ))

  private val customTypes: Map[String, Sort] = Map[String,Sort]().withDefault { name: String =>
    ctxt.mkUninterpretedSort(name)
  }

  private val translateSort: Map[SymbolicSort, Sort] = Map[SymbolicSort, Sort]().withDefault {
    case SortCustom(typ) =>
      typ match {
        case InputAst.SimpleType(name, source) =>
          customTypes(name)
        case InputAst.IdType(name, source) =>
          customTypes(name)
        case _ =>
          throw new RuntimeException(s"unsupported type $typ")
      }
    case SortInt() => ctxt.getIntSort
    case SortBoolean() => ctxt.getBoolSort
    case SortCallId() => callIdSort
    case SortTxId() => transactionIdSort
    case SortTransactionStatus() => transactionStatusSort.dt
    case SortInvocationId() => ctxt.getIntSort
    case SortCall() => callSort
    case SortInvocationInfo() => invocationInfoDatatype.dt
    case SortInvocationRes() => returnDatatype.dt
    case SortMap(keySort, valueSort) => ctxt.mkArraySort(translateSort(keySort), translateSort(valueSort))
    case SortSet(valueSort) => ctxt.mkSetSort(translateSort(valueSort))
    case SortOption(valueSort) => optionSorts(translateSort(valueSort)).dt
    case SortUid() => uidSort
  }


  def userDefinedConstructor(typ: InTypeExpr, constructorName: String): Constructor = ???


  def invocationInfoConstructor(procname: String): Constructor =
    invocationInfoDatatype.constructors(procname)

  // de-bruijn-indexes of variables and so on
  case class TranslationContext(
    indexes: Map[SymbolicVariable[_], Int] = Map()
  ) {
    def pushVariable(v: SymbolicVariable[_ <: SymbolicSort]): TranslationContext =
      this.copy(
        indexes.mapValues(_ + 1) + (v -> 0)
      )

  }

  def freshContext(): TranslationContext = TranslationContext()

  def translateBool(expr: SVal[SortBoolean])(implicit trC: TranslationContext): BoolExpr = {
    translateExpr(expr).asInstanceOf[BoolExpr]
  }

  def translateMap[K <: SymbolicSort, V <: SymbolicSort](expr: SVal[SortMap[K, V]])(implicit trC: TranslationContext): ArrayExpr = {
    translateExpr(expr).asInstanceOf[ArrayExpr]
  }

  def translateSet[T <: SymbolicSort](expr: SVal[SortSet[T]])(implicit trC: TranslationContext): ArrayExpr = {
    translateExpr(expr).asInstanceOf[ArrayExpr]
  }

  def translateInt(expr: SVal[SortInt])(implicit trC: TranslationContext): ArithExpr = {
    translateExpr(expr).asInstanceOf[ArithExpr]
  }

  def isTrue(expr: Expr): BoolExpr =
    ctxt.mkEq(expr, ctxt.mkTrue())

  def translateExpr[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): Expr = expr match {
    case ConcreteVal(value) =>
      value match {
        case b: Boolean => ctxt.mkBool(b)
        case i: BigInt => ctxt.mkInt(i.toString())
        case _ =>
          throw new RuntimeException(s"unhandled concrete value $value (${value.getClass})")
      }
    case sv@SymbolicVariable(name, typ) =>
      trC.indexes.get(sv) match {
        case Some(i) =>
          ctxt.mkBound(i, translateSort(typ))
        case None =>
          ctxt.mkConst(name, translateSort(typ))
      }
    case SEq(left, right) =>
      ctxt.mkEq(translateExpr(left), translateExpr(right))
    case SNotEq(left, right) =>
      ctxt.mkNot(ctxt.mkEq(translateExpr(left), translateExpr(right)))
    case n@SNone() =>
      ctxt.mkApp(optionSorts(translateSort(n.typ.valueSort)).none.ConstructorDecl())
    case n@SSome(value) =>
      ctxt.mkApp(optionSorts(translateSort(n.typ.valueSort)).some.ConstructorDecl(), translateExpr(value))
    case s: SOptionMatch[_,_] =>
      ctxt.mkFreshConst()
    case SMapGet(map, key) =>
      ctxt.mkSelect(translateMap(map), translateExpr(key))
    case value: SymbolicMap[_, _] =>
      value match {
        case SymbolicMapEmpty(defaultValue) =>
          ctxt.mkConstArray(translateSort(value.typ.keySort), translateExpr(defaultValue))
        case SymbolicMapVar(v) =>
          translateExpr(v)
        case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
          ctxt.mkStore(translateMap(baseMap), translateExpr(updatedKey), translateExpr(newValue))
        case m: SymbolicMapUpdatedConcrete[_, _, _] =>
          translateExpr(m.toSymbolicUpdates())
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetInsert(set, v) =>
          ctxt.mkSetAdd(translateSet(set), translateExpr(v))
        case SSetEmpty() =>
          ctxt.mkEmptySet(translateSort(value.typ.valueSort))
        case SSetVar(v) =>
          translateExpr(v)
      }
    case SSetContains(set, v) =>
      ctxt.mkSetMembership(translateExpr(v), translateSet(set))
    case QuantifierExpr(quantifier, variable, body) =>
      val universal = quantifier == QForall()
      ctxt.mkQuantifier(
        universal,
        Array[Sort](translateSort(variable.typ)),
        Array(ctxt.mkSymbol(variable.name)),
        translateExpr(body)(trC.pushVariable(variable)),
        0,
        Array[Pattern](),
        Array[Expr](),
        null,
        null
      )
    case Committed() =>
      ctxt.mkApp(transactionStatusSort.committed.ConstructorDecl())
    case Uncommitted() =>
      ctxt.mkApp(transactionStatusSort.uncommitted.ConstructorDecl())
    case SBool(value) =>
      ctxt.mkBool(value)
    case SNot(value) =>
      ctxt.mkNot(translateBool(value))
    case SAnd(left, right) =>
      ctxt.mkAnd(translateBool(left), translateBool(right))
    case SOr(left, right) =>
      ctxt.mkOr(translateBool(left), translateBool(right))
    case SImplies(left, right) =>
      ctxt.mkImplies(translateBool(left), translateBool(right))
    case SDatatypeValue(typ, constructorName, values) =>
      ctxt.mkApp(userDefinedConstructor(typ, constructorName).ConstructorDecl(), values.map(translateExpr): _*)
    case SInvocationInfo(procname, args) =>
      ctxt.mkApp(invocationInfoConstructor(procname).ConstructorDecl(), args.map(translateExpr): _*)
    case MapDomain(map) =>
      // to calculate the domain of a map we calculate
      // fun x -> m(x) != none
      val is_none = optionSorts(translateSort(map.typ.valueSort.valueSort)).none.getTesterDecl
      val keySort = translateSort(map.typ.keySort)
      ctxt.mkLambda(Array(keySort), Array(ctxt.mkSymbol("x")),
        ctxt.mkNot(isTrue(ctxt.mkApp(is_none, ctxt.mkSelect(translateMap(map), ctxt.mkBound(0, keySort))))));
    case IsSubsetOf(left, right) =>
      ctxt.mkSetSubset(translateSet(left), translateSet(right))
    case SReturnVal(proc, v) =>
      ctxt.mkApp(returnDatatype.constructors(proc).ConstructorDecl(), translateExpr(v))
    case SLessThanOrEqual(x, y) =>
      ctxt.mkLe(translateInt(x), translateInt(y))
    case SLessThan(x, y) =>
      ctxt.mkLt(translateInt(x), translateInt(y))
  }


}
