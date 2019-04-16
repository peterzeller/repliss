package crdtver.symbolic.smt

import crdtver.symbolic.smt.Smt.{ApplyConstructor, ApplySelector}

/**
  *
  */
object Smt {



  sealed abstract class SmtExpr {
    def children: Iterable[SmtExpr] = List()
  }

  sealed abstract class SmtExprNode(subExpressions: SmtExpr*) extends SmtExpr {
    override def children: Iterable[SmtExpr] = subExpressions
  }


//  case class SmtApply(func: SmtFunction, args: List[SmtExpr]) extends SmtExpr
//
//  def SmtApply(func: SmtFunction, args: SmtExpr*): SmtApply = SmtApply(func, args.toList)


  sealed abstract class SmtFunction

  sealed abstract class Type


  // uninterpreted sort
  case class Sort(name: String) extends Type

  case class Datatype(
    name: String,
    constructors: List[DatatypeConstructor]
  ) extends Type {
    def getConstructor(constructorName: String): DatatypeConstructor =
      constructors.find(_.name == name).getOrElse(throw new RuntimeException(s"Constructor $name not found in $this"))

  }

  case class DatatypeConstructor(name: String, args: List[Variable])

  case class Variable(name: String, typ: Type) extends SmtExpr


  case class IntegerType() extends Type

  case class BoolType() extends Type

  case class ArrayType(keyType: Type, valueType: Type) extends Type

  case class SetType(elementType: Type) extends Type

  case class Const(b: Boolean) extends SmtExpr

  case class ConstI(i: BigInt) extends SmtExpr



  case class Equals(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right) {
    override def children: Iterable[SmtExpr] = List(left, right)
  }

  case class Not(of: SmtExpr) extends SmtExprNode(of)

  case class ApplyConstructor(dt: Datatype, constructor: DatatypeConstructor, args: SmtExpr*) extends SmtExprNode(args: _*) {
    require(dt.constructors.contains(constructor))
    require(args.length == constructor.args.length)
  }


  def ApplyConstructor(dt: Datatype, constructor: String, args: SmtExpr*): ApplyConstructor =
    ApplyConstructor(dt, dt.getConstructor(constructor), args: _*)

  def ApplyConstructor(dt: Datatype, constructor: String, args: List[SmtExpr]): ApplyConstructor =
      ApplyConstructor(dt, dt.getConstructor(constructor), args: _*)

  case class ApplySelector(dt: Datatype, constructor: DatatypeConstructor, variable: Variable, expr: SmtExpr) extends SmtExprNode(expr) {
    require(dt.constructors.contains(constructor))
    require(constructor.args.contains(variable))
  }

  def ApplySelector(dt: Datatype, variableName: String, expr: SmtExpr): ApplySelector = {
    (for {
      c <- dt.constructors
      arg <- c.args
      if arg.name == variableName
    } yield ApplySelector(dt, c, arg, expr)).headOption.getOrElse(throw new RuntimeException(s"Variable $variableName not found in $dt"))
  }

  case class IfThenElse(cond: SmtExpr, ifTrue: SmtExpr, ifFalse: SmtExpr) extends SmtExprNode(cond, ifTrue, ifFalse)


  case class ApplyTester(dt: Datatype, constructor: DatatypeConstructor, expr: SmtExpr) extends SmtExprNode(expr) {
    require(dt.constructors.contains(constructor))
  }

  def ApplyTester(os: Datatype, constructor: String, expr: SmtExpr): ApplyTester =
    ApplyTester(os, os.getConstructor(constructor), expr)

  case class MapSelect(map: SmtExpr, key: SmtExpr) extends SmtExprNode(map, key)

  case class ConstantMap(defaultValue: SmtExpr) extends SmtExprNode(defaultValue)

  case class MapStore(map: SmtExpr, key: SmtExpr, newValue: SmtExpr) extends SmtExprNode(map, key, newValue)

  case class SetSingleton(value: SmtExpr) extends SmtExprNode(value)

  case class SetInsert(set: SmtExpr, values: List[SmtExpr]) extends SmtExprNode(set :: values: _*)

  case class EmptySet(valueType: Type) extends SmtExpr

  case class Union(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Member(value: SmtExpr, set: SmtExpr) extends SmtExprNode(value, set)

  sealed abstract class Quantifier

  case class Forall() extends Quantifier

  case class Exists() extends Quantifier

  case class QuantifierExpr(quantifier: Quantifier, variable: Variable, expr: SmtExpr) extends SmtExprNode(expr)

  case class And(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)
  case class Or(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)
  case class Implies(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class IsSubsetOf(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Leq(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Lt(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Distinct(elems: List[SmtExpr]) extends SmtExpr {
    override def children: Iterable[SmtExpr] = elems
  }

}
