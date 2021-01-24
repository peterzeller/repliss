package crdtver.testing

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{AssertStmt, SourceTrace}
import crdtver.testing.Interpreter.{EvalExprInfo, InvariantViolationException, State}

class AssertionViolatedException(a: AssertStmt, state: State, info: List[EvalExprInfo]) extends InvariantViolationException(
  TypedAst.InInvariantDecl(a.source, "Assertion", false, a.expr),
  state, info
) {

}
