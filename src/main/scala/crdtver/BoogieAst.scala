package crdtver

object BoogieAst {


  sealed abstract class Element


  case class Program(declarations: List[Declaration])

  sealed abstract class Declaration extends Element


  case class TypeDecl(
    name: String,
    attributes: List[Attribute] = List()) extends Declaration


  sealed abstract class NamedDeclaration(name: String) extends Declaration

  case class ConstantDecl(name: String, typ: TypeExpr, isUnique: Boolean)
    extends NamedDeclaration(name)

  case class FuncDecl(
    name: String,
    arguments: List[VarDecl],
    resultType: TypeExpr,
    attributes: List[Attribute] = List(),
    implementation: Option[Expr] = None)
    extends NamedDeclaration(name)

  case class Attribute(name: String, arguments: List[Either[String, Expr]] = List())

  case class VarDecl(name: String, typ: TypeExpr)

  case class Axiom(expr: Expr) extends Declaration

  case class GlobalVariable(name: String, typ: TypeExpr)
    extends NamedDeclaration(name)


  case class Procedure(name: String,
    inParams: List[VarDecl],
    outParams: List[VarDecl],
    requires: List[Requires],
    modifies: List[IdentifierExpr],
    ensures: List[Ensures],
    body: Statement)
    extends NamedDeclaration(name)


  case class Requires(isFree: Boolean, condition: Expr)

  case class Ensures(isFree: Boolean, condition: Expr)


  sealed abstract class TypeExpr extends Element {
    def ::(name: String) = VarDecl(name, this)
  }

  case class TypeBool() extends TypeExpr

  case class MapType(argsTypes: List[TypeExpr], resultType: TypeExpr) extends TypeExpr

  case class FunctionType(argsTypes: List[TypeExpr], resultType: TypeExpr) extends TypeExpr

  case class SimpleType(name: String) extends TypeExpr


  sealed abstract class Expr extends Element {
    def ==>(right: Expr) = FunctionCall("==>", List(this, right))

    def &&(right: Expr) = FunctionCall("&&", List(this, right))

    def +(right: Expr) = FunctionCall("+", List(this, right))

    def ||(right: Expr) = FunctionCall("||", List(this, right))

    def ===(right: Expr) = FunctionCall("==", List(this, right))

    def !==(right: Expr) = FunctionCall("!=", List(this, right))

    def <==>(right: Expr) = FunctionCall("<==>", List(this, right))

    def unary_!() = FunctionCall("!", List(this))

    def >=(right: Expr) = FunctionCall(">=", List(this, right))
    def >(right: Expr) = FunctionCall(">", List(this, right))
    def <=(right: Expr) = FunctionCall("<=", List(this, right))
    def <(right: Expr) = FunctionCall("<", List(this, right))

    def get(indexes: Expr*) = Lookup(this, indexes.toList)
  }


  case class IdentifierExpr(name: String) extends Expr {
    def $(args: Expr*) = FunctionCall(name, args.toList)
  }

  implicit def string2Identifier(s: String): IdentifierExpr = IdentifierExpr(s)


  case class FunctionCall(name: String, args: List[Expr]) extends Expr

  case class Lookup(mapExpr: Expr, args: List[Expr]) extends Expr


  def Old(expr: Expr) = FunctionCall("old", List(expr))

  case class Forall(vars: List[VarDecl], expr: Expr) extends Expr

  def Forall(vars: VarDecl, expr: Expr): Forall = Forall(List(vars), expr)

  case class Exists(vars: List[VarDecl], expr: Expr) extends Expr

  def Exists(vars: VarDecl, expr: Expr): Exists = Exists(List(vars), expr)

  case class BoolConst(boolVal: Boolean) extends Expr

  case class IntConst(intVal: BigInt) extends Expr


  sealed abstract class Statement

  case class Block(stmts: List[Statement]) extends Statement

  def makeBlock(stmts: Statement*): Statement = {
    Block(stmts.toList.flatMap(getStatements))
  }

  def makeBlock(stmts: List[Statement]): Statement = {
    Block(stmts.toList.flatMap(getStatements))
  }

  private def getStatements(s: Statement): List[Statement] = s match {
    case Block(ls) => ls.flatMap(getStatements)
    case _ => List(s)
  }

  def Block(stmts: Statement*): Block = Block(stmts.toList)

  case class LocalVar(name: String, typ: TypeExpr) extends Statement

  case class IfStmt(condition: Expr, ifTrue: Statement, ifFalse: Statement) extends Statement

  case class ProcCall(resultVar: Option[String], procname: String, arguments: List[Expr]) extends Statement

  case class Assignment(variable: String, expr: Expr) extends Statement

  case class Assert(expr: Expr, attributes: List[Attribute] = List()) extends Statement

  case class Assume(expr: Expr, attributes: List[Attribute] = List()) extends Statement

}
