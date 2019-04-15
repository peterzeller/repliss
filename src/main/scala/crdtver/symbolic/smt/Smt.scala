package crdtver.symbolic.smt

/**
  *
  */
object Smt {

  sealed abstract class SmtExpr


  case class SmtApply(func: SmtFunction, args: List[SmtExpr])


  sealed abstract class SmtFunction

  sealed abstract class Type


  // uninterpreted sort
  case class Sort(name: String) extends Type

  case class Datatype(
    name: String,
    constructors: List[DatatypeConstructor]
  ) extends Type

  case class DatatypeConstructor(name: String, args: List[Variable])

  case class Variable(name: String, typ: Type)


  case class IntegerType() extends Type

  case class BoolType() extends Type

  case class ArrayType(keyType: Type, valueType: Type) extends Type

  case class SetType(elementType: Type) extends Type

}
