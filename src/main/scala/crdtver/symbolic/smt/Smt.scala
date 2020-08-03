package crdtver.symbolic.smt

import crdtver.symbolic.smt.Smt.{ApplyConstructor, ApplySelector}
import crdtver.symbolic.smt.SmtPrinter.PrintContext
import crdtver.utils.PrettyPrintDoc.Doc
import edu.nyu.acsys.CVC4.{Expr, Kind}

/**
 *
 */
object Smt {


  sealed abstract class SmtExpr {
    def calcType: Type = this match {
      case node: SmtExprNode => node match {
        case Equals(left, right) => BoolType()
        case Not(of) => BoolType()
        case ApplyConstructor(dt, constructor, args) => dt
        case ApplySelector(dt, constructor, variable, expr) => variable.typ
        case IfThenElse(cond, ifTrue, ifFalse) => ifTrue.calcType
        case ApplyTester(dt, constructor, expr) => BoolType()
        case MapSelect(map, key) => map.calcType.asInstanceOf[ArrayType].valueType
        case ConstantMap(keyType, defaultValue) => ArrayType(keyType, defaultValue.calcType)
        case MapStore(map, key, newValue) => map.calcType
        case SetSingleton(value) => SetType(value.calcType)
        case SetInsert(set, values) => set.calcType
        case Union(left, right) => left.calcType
        case QuantifierExpr(quantifier, variable, expr) => BoolType()
        case And(left, right) => BoolType()
        case Or(left, right) => BoolType()
        case Implies(left, right) => BoolType()
        case IsSubsetOf(left, right) => BoolType()
        case SetContains(element, set) => BoolType()
        case Leq(left, right) => BoolType()
        case Lt(left, right) => BoolType()
      }
      case Variable(name, typ) => typ
      case Const(b) => BoolType()
      case ConstI(i) => IntegerType()
      case EmptySet(valueType) =>
        SetType(valueType)
      case Distinct(elems) =>
        BoolType()
      case OpaqueExpr(kind, expr) =>
        ???
    }

    def children: Iterable[SmtExpr] = List()
  }

  sealed abstract class SmtExprNode(subExpressions: SmtExpr*) extends SmtExpr {
    override def children: Iterable[SmtExpr] = subExpressions
  }


  //  case class SmtApply(func: SmtFunction, args: List[SmtExpr]) extends SmtExpr
  //
  //  def SmtApply(func: SmtFunction, args: SmtExpr*): SmtApply = SmtApply(func, args.toList)


  sealed abstract class SmtFunction

  sealed abstract class Type {
    def typeName(): String = this match {
      case Smt.Sort(name) => name
      case Datatype(name, _) => name
      case Smt.IntegerType() => "integer"
      case Smt.BoolType() => "bool"
      case Smt.ArrayType(keyType, valueType) => s"array_from_${keyType.typeName()}_to_${valueType.typeName()}"
      case Smt.SetType(elementType) => s"set_of_${elementType.typeName()}"
    }

  }


  // uninterpreted sort
  case class Sort(name: String) extends Type

  case class Datatype(
    name: String,
    constructors: List[DatatypeConstructor]
  ) extends Type {
    def getConstructor(constructorName: String): DatatypeConstructor =
      constructors.find(_.name == constructorName).getOrElse(throw new RuntimeException(s"Constructor $constructorName not found in $this"))

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

  case class ApplyConstructor(dt: Datatype, constructor: DatatypeConstructor, args: List[SmtExpr]) extends SmtExprNode(args: _*) {
    require(dt.constructors.contains(constructor))
    require(args.length == constructor.args.length)

    override def toString: String = s"${dt.name}.${constructor.name}(${args.mkString(", ")})"
  }

  def ApplyConstructor(dt: Datatype, constructor: DatatypeConstructor, args: SmtExpr*): ApplyConstructor =
    ApplyConstructor(dt, constructor, args.toList)

  def ApplyConstructor(dt: Datatype, constructor: String, args: SmtExpr*): ApplyConstructor =
    ApplyConstructor(dt, dt.getConstructor(constructor), args.toList)

  def ApplyConstructor(dt: Datatype, constructor: String, args: List[SmtExpr]): ApplyConstructor =
    ApplyConstructor(dt, dt.getConstructor(constructor), args)

  case class ApplySelector(dt: Datatype, constructor: DatatypeConstructor, variable: Variable, expr: SmtExpr) extends SmtExprNode(expr) {
    require(dt.constructors.contains(constructor))
    require(constructor.args.contains(variable))
  }


  case class IfThenElse(cond: SmtExpr, ifTrue: SmtExpr, ifFalse: SmtExpr) extends SmtExprNode(cond, ifTrue, ifFalse)


  case class ApplyTester(dt: Datatype, constructor: DatatypeConstructor, expr: SmtExpr) extends SmtExprNode(expr) {
    require(dt.constructors.contains(constructor))
  }

  case class MapSelect(map: SmtExpr, key: SmtExpr) extends SmtExprNode(map, key)

  case class ConstantMap(keyType: Type, defaultValue: SmtExpr) extends SmtExprNode(defaultValue)

  case class MapStore(map: SmtExpr, key: SmtExpr, newValue: SmtExpr) extends SmtExprNode(map, key, newValue)

  case class SetSingleton(value: SmtExpr) extends SmtExprNode(value)

  case class SetInsert(set: SmtExpr, values: List[SmtExpr]) extends SmtExprNode(set :: values: _*)

  case class EmptySet(valueType: Type) extends SmtExpr

  case class Union(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  sealed abstract class Quantifier

  case class Forall() extends Quantifier

  case class Exists() extends Quantifier

  case class QuantifierExpr(quantifier: Quantifier, variable: Variable, expr: SmtExpr) extends SmtExprNode(expr)

  case class And(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Or(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Implies(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class IsSubsetOf(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class SetContains(element: SmtExpr, set: SmtExpr) extends SmtExprNode(element, set)

  case class Leq(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Lt(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Distinct(elems: List[SmtExpr]) extends SmtExpr {
    override def children: Iterable[SmtExpr] = elems
  }

  case class OpaqueExpr(kind: Any, expr: Any) extends SmtExpr

  case class NamedConstraint(description: String, constraint: SmtExpr)

}
