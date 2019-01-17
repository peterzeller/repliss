package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.InExpr

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

  def translate[T <: SymbolicSort](expr: InExpr)(implicit sort: T, ctxt: SymbolicContext): SVal[T] = {
    val res: SVal[_] = expr match {
      case InputAst.VarUse(source, typ, name) =>
        ctxt.lookupLocalVariable(name)
      case InputAst.BoolConst(source, typ, value) =>
        SBool(value)
      case InputAst.IntConst(source, typ, value) =>
        ConcreteVal(value)(SortInt())
      case expr: InputAst.CallExpr => expr match {
        case InputAst.FunctionCall(source, typ, functionName, args) => ???
        case InputAst.ApplyBuiltin(source, typ, function, args) => ???
      }
      case InputAst.QuantifierExpr(source, typ, quantifier, vars, e) =>
        var res = translate(e)(SortBoolean(), ctxt)
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
    cast(res)
  }

  def cast[T <: SymbolicSort](e: SVal[_])(implicit sort: T): SVal[T] = {
    if (e.typ != sort) {
      throw new RuntimeException(s"Expected expression of type $sort, but got $e of type ${e.typ}")
    }
    e.asInstanceOf[SVal[T]]
  }


}
