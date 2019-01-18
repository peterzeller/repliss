package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.{ApplyBuiltin, InExpr}

object ExprTranslation {

  def translateType(typ: InputAst.InTypeExpr)(implicit ctxt: SymbolicContext): SymbolicSort = typ match {
    case InputAst.AnyType() => ???
    case InputAst.UnknownType() => ???
    case InputAst.BoolType() => SortBoolean()
    case InputAst.IntType() => SortInt()
    case InputAst.CallIdType() => SortCallId()
    case InputAst.InvocationIdType() => SortInvocationId()
    case InputAst.TransactionIdType() => SortTxId()
    case InputAst.InvocationInfoType() => SortInvocationInfo()
    case InputAst.InvocationResultType() => ???
    case InputAst.SomeOperationType() => ???
    case InputAst.OperationType(name, source) => ???
    case InputAst.FunctionType(argTypes, returnType, source) => ???
    case InputAst.SimpleType(name, source) => ???
    case InputAst.IdType(name, source) => ???
    case InputAst.UnresolvedType(name, source) => ???
  }

  def translateBuiltin(expr: ApplyBuiltin)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[_] = {
    val args: List[SVal[_]] = expr.args.map(translateUntyped)
    expr.function match {
      case InputAst.BF_isVisible() =>
        ???
      case InputAst.BF_happensBefore() =>
        ???
      case InputAst.BF_sameTransaction() =>
        ???
      case InputAst.BF_less() =>
        SLessThan(cast(args(0)), cast(args(1)))
      case InputAst.BF_lessEq() =>
        SLessThanOrEqual(cast(args(0)), cast(args(1)))
      case InputAst.BF_greater() =>
        SLessThan(cast(args(1)), cast(args(0)))
      case InputAst.BF_greaterEq() =>
        SLessThanOrEqual(cast(args(1)), cast(args(0)))
      case InputAst.BF_equals() =>
        ???
      case InputAst.BF_notEquals() =>
        ???
      case InputAst.BF_and() =>
        SAnd(cast(args(0)), cast(args(1)))
      case InputAst.BF_or() =>
        ???
      case InputAst.BF_implies() =>
        ???
      case InputAst.BF_not() =>
        ???
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
        ???
      case InputAst.BF_getInfo() =>
        ???
      case InputAst.BF_getResult() =>
        ???
      case InputAst.BF_getOrigin() =>
        ???
      case InputAst.BF_getTransaction() =>
        ???
      case InputAst.BF_inCurrentInvoc() =>
        ???
    }
  }

  def translate[T <: SymbolicSort](expr: InExpr)(implicit sort: T, ctxt: SymbolicContext, state: SymbolicState): SVal[T] = {
    val res: SVal[_] = translateUntyped(expr)
    cast(res)
  }

  private def translateUntyped[T <: SymbolicSort](expr: InExpr)(implicit  ctxt: SymbolicContext, state: SymbolicState): SVal[_] = {
    expr match {
      case InputAst.VarUse(source, typ, name) =>
        state.localState(ProgramVariable(name))
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
        var res = translate(e)(SortBoolean(), ctxt, state)
        val q = quantifier match {
          case InputAst.Forall() => QForall()
          case InputAst.Exists() => QExists()
        }
        for (v <- vars) {
          val v2 = ctxt.makeVariable(v.name.name)(translateType(v.typ))
          QuantifierExpr(q, v2, translate(e))
        }
        res
    }
  }

  def cast[T <: SymbolicSort](e: SVal[_])(implicit sort: T, state: SymbolicState): SVal[T] = {
    if (e.typ != sort) {
      throw new RuntimeException(s"Expected expression of type $sort, but got $e of type ${e.typ}")
    }
    e.asInstanceOf[SVal[T]]
  }


}
