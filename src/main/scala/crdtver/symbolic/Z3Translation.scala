package crdtver.symbolic

import com.microsoft.z3._


/**
  *
  */
class Z3Translation {


  val ctxt = new Context()


  private case class Z3OptionType(
    dt: DatatypeSort,
    none: Constructor,
    some: Constructor
  ) {}

  private val optionSorts: Map[SymbolicSort, Z3OptionType] = Map[SymbolicSort, Z3OptionType]().withDefault((sort: SymbolicSort) => {
    val none = ctxt.mkConstructor("None", "is_none", Array[String](), Array[Sort](), null)
    val some = ctxt.mkConstructor("Some", "is_some", Array("value"), Array(translateSort(sort)), null)
    val dt = ctxt.mkDatatypeSort("Option", Array(none, some))
    Z3OptionType(dt, none, some)
  })

  case class TransactionStatusSort(dt: DatatypeSort, committed: Constructor, uncommitted: Constructor)

  private lazy val transactionStatusSort: TransactionStatusSort = {
    val committed = ctxt.mkConstructor("Committed", "is_committed", Array[String](), Array[Sort](), null)
    val uncommitted = ctxt.mkConstructor("Uncommitted", "is_uncommitted", Array[String](), Array[Sort](), null)
    val dt = ctxt.mkDatatypeSort("TransactionStatus", Array(committed, uncommitted))
    TransactionStatusSort(dt, committed, uncommitted)
  }

  private val translateSort: Map[SymbolicSort, Sort] = Map[SymbolicSort, Sort]().withDefault {
    case SortValue() => ???
    case SortInt() => ctxt.getIntSort
    case SortBoolean() => ctxt.getBoolSort
    case SortCustom(name) => ???
    case SymbolicStateSort() => ???
    case SortCallId() => ???
    case SortTxId() => ???
    case SortTransactionStatus() => ???
    case SortInvocationId() => ???
    case SortCall() => ???
    case SortUid() => ???
    case SortInvocationInfo() => ???
    case SortMap(keySort, valueSort) => ctxt.mkArraySort(translateSort(keySort), translateSort(valueSort))
    case SortSet(valueSort) => ctxt.mkSetSort(translateSort(valueSort))
    case SortOption(valueSort) => optionSorts(valueSort).dt
  }


  def userDefinedConstructor(constructorName: String): Constructor = ???


  def invocationInfoConstructor(procname: String): Constructor = ???

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
      ctxt.mkApp(optionSorts(n.typ.valueSort).none.ConstructorDecl())
    case n@SSome(value) =>
      ctxt.mkApp(optionSorts(n.typ.valueSort).none.ConstructorDecl(), translateExpr(value))
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
    case SDatatypeValue(constructorName, values) =>
      ctxt.mkApp(userDefinedConstructor(constructorName).ConstructorDecl(), values.map(translateExpr): _*)
    case SInvocationInfo(procname, args) =>
      ctxt.mkApp(invocationInfoConstructor(procname).ConstructorDecl(), args.map(translateExpr): _*)
    case MapDomain(map) =>
      // to calculate the domain of a map we calculate
      // fun x -> m(x) != none
      val is_none = optionSorts(map.typ.valueSort.valueSort).none.getTesterDecl
      val keySort = translateSort(map.typ.keySort)
      ctxt.mkLambda(Array(keySort), Array(ctxt.mkSymbol("x")),
        ctxt.mkNot(isTrue(ctxt.mkApp(is_none, ctxt.mkSelect(translateMap(map), ctxt.mkBound(0, keySort))))));
    case IsSubsetOf(left, right) =>
      ctxt.mkSetSubset(translateSet(left), translateSet(right))
  }


}
