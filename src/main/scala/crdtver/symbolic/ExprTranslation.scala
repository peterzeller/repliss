package crdtver.symbolic

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.{InputAst, TypedAst}
import crdtver.language.TypedAst._
import crdtver.symbolic

object ExprTranslation {

  def translateType(typ: TypedAst.InTypeExpr)(implicit ctxt: SymbolicContext): SymbolicSort =
    typ match {
      case CallIdType() => SortCallId()
      case BoolType() => SortBoolean()
      case IntType() => SortInt()
      case InvocationResultType() =>
        SortInvocationRes()
      case FunctionType(argTypes, returnType, kind) => ???
      case TransactionIdType() => SortTxId()
      case InvocationInfoType() => SortInvocationInfo()
      case AnyType() => ???
      case st: IdType =>
        SortCustomUninterpreted(st.name)
      case st: SimpleType =>
        ctxt.getCustomType(st)
      case SomeOperationType() => SortCall()
      case OperationType(name) => SortCall()
      case TypedAst.InvocationIdType() => SortInvocationId()
    }

  /** determines the invocation of a call */
  def callInvocation(cId: SVal[SortCallId])(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SortOption[SortInvocationId]] = {
    val tx = ctxt.makeVariable[SortTxId]("tx")
    val i = ctxt.makeVariable[SortInvocationId]("i")
    SOptionMatch(
      state.callOrigin.get(cId),
      tx,
      state.transactionOrigin.get(tx),
      SNone(SortInvocationId())
    )
  }

  def translateBuiltin(expr: ApplyBuiltin)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[_ <: SymbolicSort] = {
    val args: List[SVal[_]] = expr.args.map(translateUntyped)
    expr.function match {
      case BF_isVisible() =>
        state.visibleCalls.contains(cast(args(0)))
      case BF_happensBefore(on) =>
        on match {
          case HappensBeforeOn.Unknown() =>
            ???
          case HappensBeforeOn.Call() =>
            val c1 = cast[SortCallId](args(0))
            val c2 = cast[SortCallId](args(1))
            callHappensBefore(c1, c2)
          case HappensBeforeOn.Invoc() =>
            val i1: SVal[SortInvocationId] = cast(args(0))
            val i2: SVal[SortInvocationId] = cast(args(1))

            val ca = ctxt.makeVariable[SortCallId]("ca")
            val cb = ctxt.makeVariable[SortCallId]("cb")

            SAnd(
              SAnd(
                SNotEq(state.invocationCalls.get(i1), SSetEmpty[SortCallId]()),
                SNotEq(state.invocationCalls.get(i2), SSetEmpty[SortCallId]())
              ),
              symbolic.QuantifierExpr(QForall(), ca,
                SImplies(
                  SSetContains(state.invocationCalls.get(i1), ca)
                  , symbolic.QuantifierExpr(QForall(), cb,
                    SImplies(
                      SSetContains(state.invocationCalls.get(i2), cb),
                      callHappensBefore(ca, cb)))))
            )
          //            // invocation
          //            val c1 = ctxt.makeVariable[SortCallId]("c1")
          //            val c2 = ctxt.makeVariable[SortCallId]("c1")
          //            // exists c1, c2 :: c1.origin == i1 && c2.origin == i2 && c1 happened before c2
          //            val existsHb =
          //              QuantifierExpr(QExists(), c1,
          //                QuantifierExpr(QExists(), c2,
          //                  SAnd(
          //                    SAnd(
          //                      SEq(callInvocation(c1), SSome(i1)),
          //                      SEq(callInvocation(c2), SSome(i2))),
          //                    callHappensBefore(c1, c2))))
          //            // forall c1, c2 :: (c1.origin == i1 && c2.origin == i2) ==> c1 happened before c2
          //            val allHb =
          //              QuantifierExpr(QForall(), c1,
          //                QuantifierExpr(QForall(), c2,
          //                  SImplies(
          //                    SAnd(
          //                      SEq(callInvocation(c1), SSome(i1)),
          //                      SEq(callInvocation(c2), SSome(i2))),
          //                    callHappensBefore(c1, c2))))
          //            SAnd(existsHb, allHb)
        }
      case BF_sameTransaction() =>
        SEq(state.callOrigin.get(cast(args(0))), state.callOrigin.get(cast(args(1))))
      case BF_less() =>
        SLessThan(cast(args(0)), cast(args(1)))
      case BF_lessEq() =>
        SLessThanOrEqual(cast(args(0)), cast(args(1)))
      case BF_greater() =>
        SLessThan(cast(args(1)), cast(args(0)))
      case BF_greaterEq() =>
        SLessThanOrEqual(cast(args(1)), cast(args(0)))
      case BF_equals() =>
        val left: SVal[SymbolicSort] = castSymbolicSort(args(0))
        val right: SVal[SymbolicSort] = castSymbolicSort(args(1))
        // automatically adapt to option types
        // TODO maybe adapt to option types in the frontend
        if (left.typ == SortOption(right.typ))
          SEq(castSymbolicSort(left), castSymbolicSort(SSome(right)))
        else if (SortOption(left.typ) == right.typ)
          SEq(castSymbolicSort(SSome(left)), castSymbolicSort(right))
        else
          SEq(left, right)
      case BF_notEquals() =>
        SNotEq(castSymbolicSort(args(0)), castSymbolicSort(args(1)))
      case BF_and() =>
        SAnd(cast(args(0)), cast(args(1)))
      case BF_or() =>
        SOr(cast(args(0)), cast(args(1)))
      case BF_implies() =>
        SImplies(cast(args(0)), cast(args(1)))
      case BF_not() =>
        SNot(cast(args(0)))
      case BF_plus() =>
        ???
      case BF_minus() =>
        ???
      case BF_mult() =>
        ???
      case BF_div() =>
        ???
      case BF_mod() =>
        ???
      case BF_getOperation() =>
        state.calls.get(cast(args(0)))
      case BF_getInfo() =>
        state.invocationOp.get(cast(args(0)))
      case BF_getResult() =>
        state.invocationRes.get(cast(args(0)))
      case BF_getOrigin() =>
        val callId = cast[SortCallId](args(0))
        val tx = ctxt.makeVariable("tx")(SortTxId())
        SOptionMatch(
          state.callOrigin.get(callId),
          tx,
          state.transactionOrigin.get(tx),
          SNone(SortInvocationId())
        )
      case BF_getTransaction() =>
        state.callOrigin.get(cast(args(0)))
      case BF_inCurrentInvoc() =>
        //        SEq(state.currentInvocation, state.transactionOrigin.get(state.callOrigin.get(cast(args(0)))))
        ???
    }
  }

  /** checks that c1 happened before c2 */
  private def callHappensBefore(c1: SVal[SortCallId], c2: SVal[SortCallId])(implicit ctxt: SymbolicContext, state: SymbolicState) = {
    SSetContains[SortCallId](state.happensBefore.get(c2), c1)
  }

  def translate[T <: SymbolicSort](expr: InExpr)(implicit sort: T, ctxt: SymbolicContext, state: SymbolicState): SVal[T] = {
    val res: SVal[_] = translateUntyped(expr)
    cast(res)
  }


  private def debugPrint(str: String): Unit = {}

  def translateUntyped(expr: InExpr)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SymbolicSort] = {
    expr match {
      case TypedAst.VarUse(source, typ, name) =>
        state.lookupLocal(name).upcast()
      case TypedAst.BoolConst(source, typ, value) =>
        SBool(value).upcast()
      case TypedAst.IntConst(source, typ, value) =>
        ConcreteVal(value)(SortInt())
      case expr: TypedAst.CallExpr => expr match {
        case TypedAst.FunctionCall(source, typ, functionName, args, kind) =>
          val translatedArgs = args.map(translateUntyped(_))
          kind match {
            case FunctionKind.FunctionKindDatatypeConstructor() =>
              val t = translateType(expr.getTyp).asInstanceOf[SortDatatype]
              SDatatypeValue(ctxt.datypeImpl(ctxt.translateSortDatatype(typ)), functionName.name, translatedArgs, t).upcast()
            case FunctionKind.FunctionKindCrdtQuery() =>
              ctxt.findQuery(functionName.name) match {
                case None =>
                  throw new RuntimeException(s"Could not find function $functionName")
                case Some(query) =>
                  // bind the parameter values:
                  var state2 = state
                  for ((p, a) <- query.params.zip(translatedArgs)) {
                    state2 = state2.withLocal(ProgramVariable(p.name.name), a)
                  }

                  query.implementation match {
                    case Some(impl) =>
                      // inline the implementation:
                      translateUntyped(impl)(ctxt, state2)
                    case None =>
                      // create a new symbolic variable for the result_
                      val result = ctxt.makeVariable(query.name.name)(translateType(query.returnType))
                      query.ensures match {
                        case Some(postCondition) =>
                          // assume the postcondition:
                          state2 = state2.withLocal(ProgramVariable("result"), result)
                          ctxt.addConstraint(s"query_${query.name}_postcondition",
                            translate(postCondition)(SortBoolean(), ctxt, state2))
                          result
                        case None =>
                          debugPrint(s"Warning: Query $functionName does not have a specification.")
                      }
                      result
                  }
              }
          }

        case bi: ApplyBuiltin =>
          translateBuiltin(bi).upcast()
      }
      case TypedAst.QuantifierExpr(source, typ, quantifier, vars, e) =>

        val q = quantifier match {
          case InputAst.Forall() => QForall()
          case InputAst.Exists() => QExists()
        }

        def tr(vars: List[InVariable], state: SymbolicState): SVal[SortBoolean] =
          vars match {
            case Nil =>
              translate(e)(implicitly, implicitly, state)
            case v :: vs =>
              val vt = ctxt.makeVariable(v.name.name)(translateType(v.typ))
              val state2 = state.withLocal(ProgramVariable(v.name.name), vt)
              symbolic.QuantifierExpr(q, vt, tr(vs, state2))
          }

        tr(vars, state).upcast()
    }
  }

  def cast[T <: SymbolicSort](e: SVal[_])(implicit sort: T, state: SymbolicState): SVal[T] = {
    if (e.typ != sort) {
      throw new RuntimeException(s"Expected expression of type $sort, but got $e of type ${e.typ}")
    }
    e.asInstanceOf[SVal[T]]
  }

  def castList[T <: SymbolicSort](es: List[SVal[_]])(implicit sort: T, state: SymbolicState): List[SVal[T]] =
    es.map(cast(_)(sort, state))

  def castSymbolicSort(e: SVal[_]): SVal[SymbolicSort] = {
    e.asInstanceOf[SVal[SymbolicSort]]
  }


}
