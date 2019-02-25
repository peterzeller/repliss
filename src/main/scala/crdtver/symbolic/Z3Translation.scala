package crdtver.symbolic

import java.util.concurrent.TimeUnit

import com.microsoft.z3
import com.microsoft.z3._
import crdtver.utils.ListExtensions.buildArray
import scalaz.Memo

import scala.concurrent.duration.Duration


/**
  *
  */
class Z3Translation(
  limitInvocations: Option[Int] = Some(2),
  limitTransactions: Option[Int] = Some(2),
  limitCalls: Option[Int] = Some(4),
  limitCustomTypes: Option[Int] = Some(2),
) extends SmtTranslation {
  override type TBoolExpr = BoolExpr
  override type TExpr = Expr
  override type TTranslationContext = this.TranslationContext

  private val ctxt: Z3Context = Z3Proxy.z3Context()


  def mkSolver(): SmtSolver = new SmtSolver() {

    val solverParams: Params = ctxt.mkParams
    solverParams.add("timeout", Duration(30, TimeUnit.SECONDS).toMillis.asInstanceOf[Int])

    val solver = ctxt.mkSolver()
    solver.setParameters(solverParams)

    override def add(translated: BoolExpr): Unit =
      solver.add(translated)

    override def check(): CheckRes =
      solver.check() match {
        case Status.UNSATISFIABLE =>
          Unsatisfiable()
        case Status.UNKNOWN =>
          Unknown()
        case Status.SATISFIABLE =>
          new Satisfiable {
            override def getModel: Model = new Model {
              val m: z3.Model = solver.getModel

              override def eval(expr: Expr, bool: Boolean): Expr =
                m.eval(expr, bool)
            }
          }
      }

    override def push(): Unit =
      solver.push()

    override def pop(): Unit =
      solver.pop()


  }

  sealed private abstract class Z3ProgramType {
    def z3type: Sort
  }

  private case class Z3Sort(
    z3type: Sort
  ) extends Z3ProgramType

  private case class Z3UninterpretedDT(
    z3type: UninterpretedSort
  ) extends Z3ProgramType {
  }

  private case class Z3DataType(
    z3type: DatatypeSort,
    constructors: Map[String, Constructor]
  ) extends Z3ProgramType {

    def getConstructor(constructorName: String): Constructor = {
      constructors.get(constructorName) match {
        case Some(c) => c
        case None =>
          throw new RuntimeException(
            s"""
               |Could not find constructor $constructorName in type ${z3type.getName}.
               |Available constructors are: ${constructors.keys.mkString(", ")}
             """.stripMargin)
      }
    }
  }

  private case class Z3OptionType(
    dt: DatatypeSort,
    none: Constructor,
    some: Constructor
  ) {}

  private val optionSorts: Sort => Z3OptionType = Memo.mutableHashMapMemo((sort: Sort) => {
    val none = ctxt.mkConstructor(s"None_$sort", s"is_none_$sort", Array[String](), Array[Sort](), null)
    val some = ctxt.mkConstructor(s"Some_$sort", s"is_some_$sort", Array("value"), Array(sort), null)
    val dt = ctxt.mkDatatypeSort(s"Option_$sort", Array(none, some))
    Z3OptionType(dt, none, some)
  })

  private case class TransactionStatusSort(dt: DatatypeSort, committed: Constructor, uncommitted: Constructor)

  //  private lazy val transactionStatusSort: TransactionStatusSort = {
  //    val committed = ctxt.mkConstructor("Committed", "is_committed", Array[String](), Array[Sort](), null)
  //    val uncommitted = ctxt.mkConstructor("Uncommitted", "is_uncommitted", Array[String](), Array[Sort](), null)
  //    val dt = ctxt.mkDatatypeSort("TransactionStatus", Array(committed, uncommitted))
  //    TransactionStatusSort(dt, committed, uncommitted)
  //  }

  private case class ReturnDatatype(
    dt: DatatypeSort,
    constructors: Map[String, Constructor],
    noneConstructor: Constructor)

  private def makeLimitedType(name: String, limit: Option[Int]): Sort =
    limit match {
      case Some(value) =>
        val constructors =
          for (x <- 0 to value) yield
            ctxt.mkConstructor(s"$name$x", s"is_$name$x", Array[String](), Array[Sort](), null)
        ctxt.mkDatatypeSort(name, constructors.toArray)
      case None =>
        ctxt.mkUninterpretedSort(name)
    }

  private lazy val callIdSort: Sort = makeLimitedType("CallId", limitCalls)
  private lazy val transactionIdSort: Sort = makeLimitedType("TxId", limitTransactions)
  private lazy val invocationIdSort: Sort = makeLimitedType("InvocationId", limitInvocations)

  // TODO make this a datatpye with all possible crdt operations
  //  lazy val callSort: DatatypeSort =
  //    ctxt.mkDatatypeSort("call", Array(
  //      ctxt.mkConstructor("call", "is_call", Array[String](), Array[Sort](), null)
  //    ))

  private val customTypes: String => Sort = Memo.mutableHashMapMemo { name: String =>
    ctxt.mkUninterpretedSort(name)
  }

  private val translateDatatypeImpl: SortDatatypeImpl => Z3DataType = Memo.mutableHashMapMemo { s: SortDatatypeImpl =>
    val constructors: Map[String, Constructor] =
      s.constructors.mapValues(c => {
        ctxt.mkConstructor(c.name, s"is_${c.name}", c.args.map(_.name).toArray, c.args.map(a => translateSort(a.typ)), null)
      }).view.force


    try {
      val dt = ctxt.mkDatatypeSort(s.name, constructors.values.toArray)
      Z3DataType(dt, constructors)
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Could not create datatype ${s.name} with constructors $constructors", e)
    }
  }

  private val translateSortCustomUninterpreted: SortCustomUninterpreted => Sort = Memo.mutableHashMapMemo { s =>
    makeLimitedType(s.name, limitCustomTypes)
  }

  private def translateSortDataType(s: SortDatatype): Z3DataType = {
    val dt = datatypeImpl(s)
    translateDatatypeImpl(dt)
  }

  private val translateSort: SymbolicSort => Sort = Memo.mutableHashMapMemo {
    case SortCustomDt(dt) =>
      translateDatatypeImpl(dt).z3type
    case s: SortCustomUninterpreted =>
      translateSortCustomUninterpreted(s)
    case s: SortDatatype =>
      translateSortDataType(s).z3type
    case SortInt() => ctxt.getIntSort
    case SortBoolean() => ctxt.getBoolSort
    case SortCallId() => callIdSort
    case SortTxId() => transactionIdSort
    //    case SortTransactionStatus() => transactionStatusSort.dt
    case SortInvocationId() => invocationIdSort
    //    case SortCall() => callSort
    case SortMap(keySort, valueSort) => ctxt.mkArraySort(translateSort(keySort), translateSort(valueSort))
    case SortSet(valueSort) => ctxt.mkSetSort(translateSort(valueSort))
    case SortOption(valueSort) => optionSorts(translateSort(valueSort)).dt
  }


  private def userDefinedConstructor(typ: SortDatatypeImpl, constructorName: String): Constructor = {
    val dt = translateDatatypeImpl(typ)
    dt.getConstructor(constructorName)
  }


  //  def invocationInfoConstructor(procname: String): Constructor =
  //    invocationInfoDatatype.constructors(procname)


  // de-bruijn-indexes of variables and so on
  case class TranslationContext(
    indexes: Map[SymbolicVariable[_], Int] = Map(),
    variableValues: Map[SymbolicVariable[_], Expr] = Map()
  ) {
    def withSpecialVariable(v: SymbolicVariable[_ <: SymbolicSort], value: Expr): TranslationContext =
      this.copy(
        variableValues = variableValues + (v -> value)
      )

    def pushVariable(v: SymbolicVariable[_ <: SymbolicSort]): TranslationContext =
      this.copy(
        indexes = indexes.mapValues(_ + 1) + (v -> 0)
      )

  }

  def freshContext(): TranslationContext = TranslationContext()

  override def translateBool(expr: SVal[SortBoolean], trC: this.TranslationContext): BoolExpr = {
    translateExprH(expr)(trC).asInstanceOf[BoolExpr]
  }

  def translateBoolH(expr: SVal[SortBoolean])(implicit trC: this.TranslationContext): BoolExpr = {
    translateExprH(expr)(trC).asInstanceOf[BoolExpr]
  }

  private def translateMap[K <: SymbolicSort, V <: SymbolicSort](expr: SVal[SortMap[K, V]])(implicit trC: TranslationContext): ArrayExpr = {
    translateExprH(expr).asInstanceOf[ArrayExpr]
  }

  private def translateSet[T <: SymbolicSort](expr: SVal[SortSet[T]])(implicit trC: TranslationContext): ArrayExpr = {
    translateExprH(expr).asInstanceOf[ArrayExpr]
  }

  private def translateInt(expr: SVal[SortInt])(implicit trC: TranslationContext): ArithExpr = {
    translateExprH(expr).asInstanceOf[ArithExpr]
  }

  private def isTrue(expr: Expr): BoolExpr =
    ctxt.mkEq(expr, ctxt.mkTrue())

  // optimization: The same SVal should yield the same expression because Z3 reuses this
  private val translationCache: ((SVal[_], TranslationContext)) => Expr =
    Memo.mutableHashMapMemo[(SVal[_], TranslationContext), Expr] { case (v, c) =>
      translateExprIntern(v.asInstanceOf[SVal[SymbolicSort]])(c)
    }

  override def translateExpr[T <: SymbolicSort](expr: SVal[T], trC: TranslationContext): Expr = {
    translateExprH(expr)(trC)
  }

  private def translateExprH[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): Expr = {
    try {
      translationCache(expr, trC)
    } catch {
      case err: Throwable =>
        throw new RuntimeException("Error when translating\n" + expr, err)
    }
  }


  private def translateExprIntern[T <: SymbolicSort](expr: SVal[T])(implicit trC: TranslationContext): Expr = expr match {
    case ConcreteVal(value) =>
      value match {
        case b: Boolean => ctxt.mkBool(b)
        case i: BigInt => ctxt.mkInt(i.toString())
        case _ =>
          throw new RuntimeException(s"unhandled concrete value $value (${value.getClass})")
      }
    case sv@SymbolicVariable(name, typ) =>
      trC.variableValues.get(sv) match {
        case Some(e) => e
        case None =>
          trC.indexes.get(sv) match {
            case Some(i) =>
              ctxt.mkBound(i, translateSort(typ))
            case None =>
              ctxt.mkConst(name, translateSort(typ))
          }
      }
    case SEq(left, right) =>
      ctxt.mkEq(translateExprH(left), translateExprH(right))
    case SNotEq(left, right) =>
      ctxt.mkNot(ctxt.mkEq(translateExprH(left), translateExprH(right)))
    case SNone(t) =>
      ctxt.mkApp(optionSorts(translateSort(t)).none.ConstructorDecl())
    case n@SSome(value) =>
      ctxt.mkApp(optionSorts(translateSort(n.typ.valueSort)).some.ConstructorDecl(), translateExprH(value))
    case s: SOptionMatch[_, _] =>
      val os = optionSorts(translateSort(s.option.typ.valueSort))
      val option = translateExprH(s.option)
      val ifNone = translateExprH(s.ifNone)
      val ifSomeValue = ctxt.mkApp(os.some.getAccessorDecls()(0), option)
      val ifSome = translateExprH(s.ifSome)(trC.withSpecialVariable(s.ifSomeVariable, ifSomeValue))
      ctxt.mkITE(
        ctxt.mkEq(ctxt.mkApp(os.some.getTesterDecl, option), ctxt.mkTrue()),
        ifNone,
        ifSome
      )
    case SMapGet(map, key) =>
      ctxt.mkSelect(translateMap(map), translateExprH(key))
    case value: SymbolicMap[_, _] =>
      value match {
        case SymbolicMapEmpty(defaultValue) =>
          ctxt.mkConstArray(translateSort(value.typ.keySort), translateExprH(defaultValue))
        case SymbolicMapVar(v) =>
          translateExprH(v)
        case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
          ctxt.mkStore(translateMap(baseMap), translateExprH(updatedKey), translateExprH(newValue))
        case m: SymbolicMapUpdatedConcrete[_, _, _] =>
          translateExprH(m.toSymbolicUpdates())
      }
    case value: SymbolicSet[_] =>
      value match {
        case SSetInsert(set, v) =>
          ctxt.mkSetAdd(translateSet(set), translateExprH(v))
        case SSetEmpty() =>
          ctxt.mkEmptySet(translateSort(value.typ.valueSort))
        case SSetVar(v) =>
          translateExprH(v)
        case SSetUnion(a, b) =>
          ctxt.mkSetUnion(translateSet(a), translateSet(b))
      }
    case SSetContains(set, v) =>
      ctxt.mkSetMembership(translateExprH(v), translateSet(set))
    case QuantifierExpr(quantifier, variable, body) =>
      val universal = quantifier == QForall()
      ctxt.mkQuantifier(
        universal,
        Array[Sort](translateSort(variable.typ)),
        Array(ctxt.mkSymbol(variable.name)),
        translateExprH(body)(trC.pushVariable(variable)),
        0,
        Array[Pattern](),
        Array[Expr](),
        null,
        null
      )
    case s@SCommitted() =>
      val z3t = translateSortDataType(s.typ)
      ctxt.mkApp(z3t.getConstructor("Committed").ConstructorDecl())
    case s@SUncommitted() =>
      val z3t = translateSortDataType(s.typ)
      ctxt.mkApp(z3t.getConstructor("Uncommitted").ConstructorDecl())
    case SBool(value) =>
      ctxt.mkBool(value)
    case SNot(value) =>
      ctxt.mkNot(translateBool(value, trC))
    case SAnd(left, right) =>
      ctxt.mkAnd(translateBoolH(left), translateBoolH(right))
    case SOr(left, right) =>
      ctxt.mkOr(translateBoolH(left), translateBoolH(right))
    case SImplies(left, right) =>
      ctxt.mkImplies(translateBoolH(left), translateBoolH(right))
    case SDatatypeValue(typ, constructorName, values, t) =>
      val tr = values.map(v => translateExprH(v))
      ctxt.mkApp(userDefinedConstructor(typ, constructorName).ConstructorDecl(), tr: _*)
    case fc@SFunctionCall(typ, name, args) =>
      throw new RuntimeException(s"translation missing for function call $fc")
    case s@SInvocationInfo(procname, args) =>
      val z3t = translateSortDataType(s.typ)
      ctxt.mkApp(z3t.getConstructor(procname).ConstructorDecl(), args.map(translateExprH): _*)
    case s@SInvocationInfoNone() =>
      val z3t = translateSortDataType(s.typ)
      val constructor = z3t.getConstructor("no_invocation")
      val constructorDecl = constructor.ConstructorDecl()
      ctxt.mkApp(constructorDecl)
    case s@SCallInfo(c, args) =>
      val z3t = translateSortDataType(s.typ)
      ctxt.mkApp(z3t.getConstructor(c).ConstructorDecl(), args.map(translateExprH): _*)
    case s: SCallInfoNone =>
      val z3t = translateSortDataType(s.typ)
      ctxt.mkApp(z3t.getConstructor("no_call").ConstructorDecl())
    case MapDomain(map) =>
      // to calculate the domain of a map we calculate
      // fun x -> m(x) != none
      val is_none = optionSorts(translateSort(map.typ.valueSort.valueSort)).none.getTesterDecl
      val keySort = translateSort(map.typ.keySort)
      ctxt.mkLambda(Array(keySort), Array(ctxt.mkSymbol("x")),
        ctxt.mkNot(isTrue(ctxt.mkApp(is_none, ctxt.mkSelect(translateMap(map), ctxt.mkBound(0, keySort))))));
    case IsSubsetOf(left, right) =>
      ctxt.mkSetSubset(translateSet(left), translateSet(right))
    case s@SReturnVal(proc, v) =>
      val z3t = translateSortDataType(s.typ)
      ctxt.mkApp(z3t.getConstructor(s"${proc}_res").ConstructorDecl(), translateExprH(v))
    case s@SReturnValNone() =>
      val z3t = translateSortDataType(s.typ)
      ctxt.mkApp(z3t.getConstructor("no_return").ConstructorDecl())
    case SLessThanOrEqual(x, y) =>
      ctxt.mkLe(translateInt(x), translateInt(y))
    case SLessThan(x, y) =>
      ctxt.mkLt(translateInt(x), translateInt(y))
    case SDistinct(args) =>
      ctxt.mkDistinct(args.map(translateExprH): _*)
  }


}
