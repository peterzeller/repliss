package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.{AnyType, ApplyBuiltin, BoolType, CallIdType, FunctionType, IdType, InExpr, IntType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType, TransactionIdType, UnknownType, UnresolvedType}

object ExprTranslation {

  def translateType(typ: InputAst.InTypeExpr)(implicit ctxt: SymbolicContext): SymbolicSort =
    typ match {
      case CallIdType() => SortCallId()
      case BoolType() => SortBoolean()
      case IntType() => SortInt()
      case InvocationResultType() =>
        SortInvocationRes()
      case FunctionType(argTypes, returnType, source) => ???
      case UnresolvedType(name, source) => ???
      case TransactionIdType() => SortTxId()
      case InvocationInfoType() => SortInvocationInfo()
      case AnyType() => ???
      case UnknownType() => ???
      case st: IdType =>
        SortValue(st)
      case st: SimpleType =>
        SortValue(st)
      case SomeOperationType() => ???
      case OperationType(name, source) => ???
      case InputAst.InvocationIdType() => SortInvocationId()
    }

  def translateBuiltin(expr: ApplyBuiltin)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[_] = {
    val args: List[SVal[_]] = expr.args.map(translateUntyped)
    expr.function match {
      case InputAst.BF_isVisible() =>
        state.visibleCalls.contains(cast(args(0)))
      case InputAst.BF_happensBefore() =>
        // TODO handle case for invocation-happens-before
        SSetContains[SortCallId](state.happensBefore.get(cast(args(1))), cast(args(0)))
      case InputAst.BF_sameTransaction() =>
        SEq(state.callOrigin.get(cast(args(0))), state.callOrigin.get(cast(args(1))))
      case InputAst.BF_less() =>
        SLessThan(cast(args(0)), cast(args(1)))
      case InputAst.BF_lessEq() =>
        SLessThanOrEqual(cast(args(0)), cast(args(1)))
      case InputAst.BF_greater() =>
        SLessThan(cast(args(1)), cast(args(0)))
      case InputAst.BF_greaterEq() =>
        SLessThanOrEqual(cast(args(1)), cast(args(0)))
      case InputAst.BF_equals() =>
        SEq(castSymbolicSort(args(0)), castSymbolicSort(args(1)))
      case InputAst.BF_notEquals() =>
        SNotEq(castSymbolicSort(args(0)), castSymbolicSort(args(1)))
      case InputAst.BF_and() =>
        SAnd(cast(args(0)), cast(args(1)))
      case InputAst.BF_or() =>
        SOr(cast(args(0)), cast(args(1)))
      case InputAst.BF_implies() =>
        SImplies(cast(args(0)), cast(args(1)))
      case InputAst.BF_not() =>
        SNot(cast(args(0)))
      case InputAst.BF_plus() =>
        ???
      case InputAst.BF_minus() =>
        ???
      case InputAst.BF_mult() =>
        ???
      case InputAst.BF_div() =>
        ???
      case InputAst.BF_mod() =>
        ???
      case InputAst.BF_getOperation() =>
        state.calls.get(cast(args(0)))
      case InputAst.BF_getInfo() =>
        state.invocationOp.get(cast(args(0)))
      case InputAst.BF_getResult() =>
        state.invocationRes.get(cast(args(0)))
      case InputAst.BF_getOrigin() =>
        state.transactionOrigin.get(cast(args(0)))
      case InputAst.BF_getTransaction() =>
        state.callOrigin.get(cast(args(0)))
      case InputAst.BF_inCurrentInvoc() =>
//        SEq(state.currentInvocation, state.transactionOrigin.get(state.callOrigin.get(cast(args(0)))))
        ???
    }
  }

  def translate[T <: SymbolicSort](expr: InExpr)(implicit sort: T, ctxt: SymbolicContext, state: SymbolicState): SVal[T] = {
    val res: SVal[_] = translateUntyped(expr)
    cast(res)
  }

  private def translateUntyped[T <: SymbolicSort](expr: InExpr)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[_] = {
    expr match {
      case InputAst.VarUse(source, typ, name) =>
        state.lookupLocal(name)
      case InputAst.BoolConst(source, typ, value) =>
        SBool(value)
      case InputAst.IntConst(source, typ, value) =>
        ConcreteVal(value)(SortInt())
      case expr: InputAst.CallExpr => expr match {
        case InputAst.FunctionCall(source, typ, functionName, args) => ???
        case bi: ApplyBuiltin =>
          translateBuiltin(bi)
      }
      case InputAst.QuantifierExpr(source, typ, quantifier, vars, e) =>

        val q = quantifier match {
          case InputAst.Forall() => QForall()
          case InputAst.Exists() => QExists()
        }
        var state2 = state
        var res: SVal[SortBoolean] => SVal[SortBoolean] = x => x
        for (v <- vars) {
          val v2 = ctxt.makeVariable(v.name.name)(translateType(v.typ))
          state2 = state2.withLocal(ProgramVariable(v.name.name), v2)
          res = e => QuantifierExpr(q, v2, e)
        }
        res(translate(e)(implicitly, implicitly, state2))
    }
  }

  def cast[T <: SymbolicSort](e: SVal[_])(implicit sort: T, state: SymbolicState): SVal[T] = {
    if (e.typ != sort) {
      throw new RuntimeException(s"Expected expression of type $sort, but got $e of type ${e.typ}")
    }
    e.asInstanceOf[SVal[T]]
  }

  def castSymbolicSort(e: SVal[_]): SVal[SymbolicSort] = {
    e.asInstanceOf[SVal[SymbolicSort]]
  }


}
