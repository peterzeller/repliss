package crdtver.language

import crdtver.language.InputAst.BuiltInFunc.BF_isVisible
import crdtver.language.TypedAst.FunctionKind.FunctionKindCrdtQuery
import crdtver.language.TypedAst._


object InvariantTransform {

  /**
    * Transforms invariants: Wraps free occurrences of visibility (including queries)
    * with a quantification over all valid contexts
    */
  def transformProg(prog: InProgram): InProgram = {
    prog.copy(
      invariants = prog.invariants.map(transformInvariant)
    )
  }

  private def transformInvariant(inv: InInvariantDecl): InInvariantDecl =
    if (hasFreeVis(inv.expr))
      inv.copy(expr = InAllValidSnapshots(inv.expr.getSource(), inv.expr))
    else
      inv


  private def hasFreeVis(expr: TypedAst.InExpr): Boolean = expr match {
    case _: IntConst => false
    case _: TypedAst.InAllValidSnapshots => false
    case ApplyBuiltin(_, _, bf, args) =>
      bf match {
        case BF_isVisible() =>
          true
        case _ =>
          args.exists(hasFreeVis)
      }
    case FunctionCall(_, _, _, args, kind) =>
      kind match {
        case FunctionKindCrdtQuery() =>
          true
        case _ =>
          args.exists(hasFreeVis)
      }
    case v: VarUse =>
      false
    case QuantifierExpr(source, quantifier, vars, ne) =>
      hasFreeVis(ne)
    case _: BoolConst =>
      false
  }


}
