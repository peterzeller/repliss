package crdtver.testing

import java.util

import crdtver.language.InputAst
import crdtver.language.InputAst.{AnyType, ApplyBuiltin, BF_and, BF_div, BF_equals, BF_getInfo, BF_getOperation, BF_getOrigin, BF_getResult, BF_greater, BF_greaterEq, BF_happensBefore, BF_implies, BF_inCurrentInvoc, BF_isVisible, BF_less, BF_lessEq, BF_minus, BF_mod, BF_mult, BF_not, BF_notEquals, BF_or, BF_plus, BF_sameTransaction, BoolConst, BoolType, CallExpr, CallIdType, Exists, Forall, FunctionCall, FunctionType, IdType, InExpr, IntConst, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, QuantifierExpr, SimpleType, SomeOperationType, UnknownType, UnresolvedType, VarUse}
import crdtver.testing.Interpreter.{LocalState, State}
import logiceval.JavaDsl._
import logiceval.ast._
import logiceval.{JavaDsl, Structure, ast}

import scala.collection.JavaConverters._


class LogicEvaluatorConv(prog: InputAst.InProgram) {

  val visibleCalls: Expr = constantUse("visibleCalls")

  val happensBefore: Expr = constantUse("happensBefore")

  val transaction: Func = new CFunc("transaction")

  val less: Func = new CFunc("less")

  val lessEq: Func = new CFunc("lessEq")

  val plus: Func = new CFunc("plus")
  val minus: Func = new CFunc("minus")
  val mult: Func = new CFunc("mult")
  val div: Func = new CFunc("div")
  val mod: Func = new CFunc("mod")

  val operation: Expr = constantUse("operation")
  val info: Expr = constantUse("info")
  val result: Expr = constantUse("result")
  val origin: Expr = constantUse("origin")

  val currentInvocation: Expr = constantUse("currentInvocation")

  def transformBuiltin(function: InputAst.BuiltInFunc, args: List[Expr]): Expr = (function, args) match {
    case (BF_isVisible(), List(x)) =>
      contains(x, visibleCalls)
    case (BF_happensBefore(), List(x, y)) =>
      contains(x, get(happensBefore, y))
    case (BF_sameTransaction(), List(x, y)) =>
      JavaDsl.eq(app(transaction, x), app(transaction, y))
    case (BF_less(), List(x, y)) =>
      app(less, x, y)
    case (BF_lessEq(), List(x, y)) =>
      app(lessEq, x, y)
    case (BF_greater(), List(x, y)) =>
      app(lessEq, y, x)
    case (BF_greaterEq(), List(x, y)) =>
      app(less, y, x)
    case (BF_equals(), List(x, y)) =>
      JavaDsl.eq(x, y)
    case (BF_notEquals(), List(x, y)) =>
      not(JavaDsl.eq(x, y))
    case (BF_and(), List(x, y)) =>
      and(x, y)
    case (BF_or(), List(x, y)) =>
      or(x, y)
    case (BF_implies(), List(x, y)) =>
      implies(x, y)
    case (BF_not(), List(x)) =>
      not(x)
    case (BF_plus(), List(x, y)) =>
      app(plus, x, y)
    case (BF_minus(), List(x, y)) =>
      app(minus, x, y)
    case (BF_mult(), List(x, y)) =>
      app(mult, x, y)
    case (BF_div(), List(x, y)) =>
      app(div, x, y)
    case (BF_mod(), List(x, y)) =>
      app(mod, x, y)
    case (BF_getOperation(), List(x)) =>
      get(operation, x)
    case (BF_getInfo(), List(x)) =>
      get(info, x)
    case (BF_getResult(), List(x)) =>
      get(result, x)
    case (BF_getOrigin(), List(x)) =>
      get(origin, x)
    case (BF_inCurrentInvoc(), List(x)) =>
      contains(currentInvocation, x)
  }

  private val t_bool = new ast.CustomType("bool")

  private val t_int = new ast.CustomType("int")

  private val t_callId = new ast.CustomType("callId")


  private val t_invocationId = new ast.CustomType("invocationId")

  private val t_invocationInfo = new ast.CustomType("invocationInfo")

  private val t_someOperation = new ast.CustomType("someOperation")

  def convertTyp(typ: InputAst.InTypeExpr): Type = typ match {
    case AnyType() =>
      ???
    case UnknownType() =>
      ???
    case BoolType() =>
      t_bool
    case IntType() =>
      t_int
    case CallIdType() =>
      t_callId
    case InvocationIdType() =>
      t_invocationId
    case InvocationInfoType() =>
      t_invocationInfo
    case InvocationResultType() =>
      val constructors = List()
      new ast.DataType("operationResult", constructors.asJava)
    case SomeOperationType() =>
      t_someOperation
    case OperationType(name, source) =>
      new ast.CustomType("operation_" + name)
    case FunctionType(argTypes, returnType, source) =>
      // new ast.FunctionType(argTypes.map(convertTyp).asJava, convertTyp(returnType))
      ???
    case SimpleType(name) =>
      new ast.CustomType("_" + name)
    case t: IdType =>
      idTypes(t)
    case UnresolvedType(name, source) =>
      ???
  }

  def convertVar(v: InputAst.InVariable): Variable = {
    new ast.Variable(v.name.name, convertTyp(v.typ))
  }

  def convertExpr(e: InExpr): Expr = e match {
    case VarUse(source, typ, name) =>
      new ast.VarUse(name)
    case BoolConst(source, typ, value) =>
      new ast.ConstantValue(value)
    case IntConst(source, typ, value) =>
      new ast.ConstantValue(value)
    case ce: CallExpr => ce match {
      case FunctionCall(source, typ, functionName, args) =>
        // TODO inline or back to interpreter?
        // back to interpreter: simpler, can use special CRDT query method
        // inline: might be possible to optimize
        app(new CFunc("query_" + functionName), args.map(convertExpr): _*)
      case ApplyBuiltin(source, typ, function, args) =>
        transformBuiltin(function, args.map(convertExpr))
    }
    case QuantifierExpr(source, typ, quantifier, vars, expr) =>
      val q = quantifier match {
        case Forall() =>
          new ast.Forall()
        case Exists() =>
          new ast.Exists()
      }
      var res: Expr = convertExpr(expr)
      for (v <- vars.reverse) {
        res = new ast.QuantifierExpr(q, convertVar(v), res)
      }
      res
  }

  def makeList(vals: AnyRef*): util.List[AnyRef] = {
    vals.asJava
  }

  def list[T](iterable: Iterable[T]): util.List[AnyRef] = {
    new util.AbstractList[AnyRef] {


      override def get(index: Int): AnyRef = {
        var c = 0
        for (v <- iterable) {
          if (c == index) {
            return v.asInstanceOf[AnyRef]
          }
          c += 1
        }
        throw new IndexOutOfBoundsException("" + index)
      }

      override def size(): Int = {
        iterable.size
      }

      override def iterator(): util.Iterator[AnyRef] = {
        iterable.iterator.map(x => x.asInstanceOf[AnyRef]).asJava
      }

    }
  }

  val boolValues: util.List[AnyRef] = makeList(java.lang.Boolean.FALSE, java.lang.Boolean.TRUE)
  lazy val intValues: util.List[AnyRef] = (0 to 1000).map(x => x.asInstanceOf[AnyRef]).asJava

  val idTypes: Map[IdType, CustomType] = prog.types.filter(_.isIdType).map(t => (IdType(t.name.name)(), new CustomType("_" + t.name))).toMap
  val idTypesR: Map[CustomType, IdType] = idTypes.map(_.swap)

  def defineStructure(localState: LocalState, inState: State): Structure = {
    new Structure {


      override def valuesForCustomType(customType: CustomType): util.List[AnyRef] = {
        if (customType == t_bool) {
          boolValues
        } else if (customType == t_int) {
          intValues
        } else if (customType == t_callId) {
          list(inState.calls.keys)
        } else if (customType == t_invocationId) {
          list(inState.invocations.keys)
        } else if (customType == t_invocationInfo) {
          list(inState.invocations.values)
        } else if (idTypesR.contains(customType)) {
          val idType = idTypesR(customType)
          list(inState.knownIds.getOrElse(idType, Set()))
        } else {
          throw new RuntimeException(s"unhandled case ${customType.getName}")
        }
      }

      override def interpretConstant(s: String, objects: Array[AnyRef]): AnyRef = {
        throw new RuntimeException(s"unhandled case $s(${objects.mkString(", ")})")
      }
    }
  }

}
