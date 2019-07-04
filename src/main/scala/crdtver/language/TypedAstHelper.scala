package crdtver.language

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.{Exists, Forall, Identifier, NoSource}
import crdtver.language.TypedAst._
import crdtver.language.crdts.UniqueName


/**
  * Helper functions for case classes in InputAst
  */

object TypedAstHelper {
  /**
    * forall quantifier function
    *
    * @param v
    * @param exp
    */
  def forall(v: InVariable, exp: InExpr): QuantifierExpr = {
    QuantifierExpr(
      source = NoSource(),
      typ = BoolType(),
      quantifier = Forall(),
      vars = List(v),
      expr = exp
    )
  }

  def isExists(v: InVariable, exp: InExpr): QuantifierExpr = {
    QuantifierExpr(
      source = NoSource(),
      typ = BoolType(),
      quantifier = Exists(),
      vars = List(v),
      expr = exp
    )
  }

  def exists(vs: List[InVariable], exp: InExpr): QuantifierExpr = {
    QuantifierExpr(
      source = NoSource(),
      typ = BoolType(),
      quantifier = Exists(),
      vars = vs,
      expr = exp
    )
  }

  def isVisible(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_isVisible(),
      args = List(exp)
    )
  }

  def happensBeforeCall(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_happensBefore(HappensBeforeOn.Call()),
      args = List(exp1, exp2)
    )
  }

  def and(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_and(),
      args = List(exp1, exp2)
    )
  }

  def and(exp: List[InExpr]): InExpr = {
    exp.reduceLeft(and)
  }

  def or(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_or(),
      args = List(exp1, exp2)
    )
  }

  def calculateOr(exp: List[InExpr]): InExpr = {
    exp.reduceLeft(or)
  }

  def implies(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_implies(),
      args = List(exp1, exp2)
    )
  }

  def not(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_not(),
      args = List(exp)
    )
  }

  def getOp(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = SomeOperationType(),
      function = BF_getOperation(),
      args = List(exp)
    )
  }

  def isEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_equals(),
      args = List(exp1, exp2)
    )
  }

  def notEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_notEquals(),
      args = List(exp1, exp2)
    )
  }

  /**
    * Create a inVariable instance
    *
    * @param c       - variable name
    * @param typExpr - type name
    */

  def makeVariable(c: UniqueName, typExpr: InTypeExpr): InVariable = {
    InVariable(
      source = NoSource(),
      name = c,
      typ = typExpr
    )
  }

  def varUse(c: UniqueName): VarUse = {
    VarUse(
      source = NoSource(),
      typ = CallIdType(),
      name = c
    )
  }

  def makeOperation(name: UniqueName, exp: InExpr*): FunctionCall = {
    FunctionCall(
      source = NoSource(),
      typ = SomeOperationType(),
      functionName = name,
      args = exp.toList,
      kind = FunctionKind.FunctionKindDatatypeConstructor()
    )
  }

  def makeOperationL(name: UniqueName, exp: List[InExpr]): FunctionCall = {
    FunctionCall(
      source = NoSource(),
      typ = SomeOperationType(),
      functionName = name,
      args = exp,
      kind = FunctionKind.FunctionKindDatatypeConstructor()
    )
  }
}