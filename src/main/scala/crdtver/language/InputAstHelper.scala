package crdtver.language

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.{ApplyBuiltin, Exists, Forall, FunctionCall, Identifier, InExpr, InTypeExpr, InVariable, NoSource, QuantifierExpr, VarUse}

/**
  * Helper functions for case classes in InputAst
  */

object InputAstHelper {
  /**
    * forall quantifier function
    *
    * @param v
    * @param exp
    */
  def forall(v: InVariable, exp: InExpr): QuantifierExpr = {
    QuantifierExpr(
      source = NoSource(),
      quantifier = Forall(),
      vars = List(v),
      expr = exp
    )
  }

  def isExists(v: InVariable, exp: InExpr): QuantifierExpr = {
    QuantifierExpr(
      source = NoSource(),
      quantifier = Exists(),
      vars = List(v),
      expr = exp
    )
  }

  def exists(vs: List[InVariable], exp: InExpr): QuantifierExpr = {
    QuantifierExpr(
      source = NoSource(),
      quantifier = Exists(),
      vars = vs,
      expr = exp
    )
  }

  def isVisible(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      function = BF_isVisible(),
      args = List(exp)
    )
  }

  def happensBefore(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      function = BF_happensBefore(HappensBeforeOn.Unknown()),
      args = List(exp1, exp2)
    )
  }

  def and(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      function = BF_and(),
      args = List(exp1, exp2)
    )
  }

  def calculateAnd(exp: List[InExpr]): InExpr = {
    exp.reduceLeft(and)
  }

  def or(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
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
      function = BF_implies(),
      args = List(exp1, exp2)
    )
  }

  def not(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      function = BF_not(),
      args = List(exp)
    )
  }

  def getOp(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      function = BF_getOperation(),
      args = List(exp)
    )
  }

  def isEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      function = BF_equals(),
      args = List(exp1, exp2)
    )
  }

  def notEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
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

  def getVariable(c: String, typExpr: InTypeExpr): InVariable = {
    InVariable(
      source = NoSource(),
      name = Identifier(NoSource(), c),
      typ = typExpr
    )
  }

  def varUse(c: String): VarUse = {
    VarUse(
      source = NoSource(),
      name = c
    )
  }

  def functionCall(name: String, exp: InExpr*): FunctionCall = {
    FunctionCall(
      source = NoSource(),
      functionName = Identifier(NoSource(), name),
      args = exp.toList
    )
  }

  def mfunctionCall(name: String, exp: List[InExpr]): FunctionCall = {
    FunctionCall(
      source = NoSource(),
      functionName = Identifier(NoSource(), name),
      args = exp
    )
  }
}
