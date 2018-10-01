package crdtver.symbolic

import crdtver.language.InputAst
import crdtver.language.InputAst.InExpr

object ExprTranslation {

  def translate[T <: SymbolicSort](expr: InExpr)(implicit sort: T, ctxt: SymbolicContext): SVal[T] = expr match {
    case InputAst.VarUse(source, typ, name) => ???
    case InputAst.BoolConst(source, typ, value) => ???
    case InputAst.IntConst(source, typ, value) => ???
    case expr: InputAst.CallExpr => expr match {
      case InputAst.FunctionCall(source, typ, functionName, args) => ???
      case InputAst.ApplyBuiltin(source, typ, function, args) => ???
    }
    case InputAst.QuantifierExpr(source, typ, quantifier, vars, e) => ???
  }

}
