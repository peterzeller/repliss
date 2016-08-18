package crdtver

object BoogieAst {


  sealed abstract class Element


  case class Program(declarations: List[Declaration])

  sealed abstract class Declaration extends Element


  case class TypeDecl(name: String) extends Declaration


  sealed abstract class NamedDeclaration(name: String) extends Declaration

  case class ConstantDecl(name: String, typ: TypeExpr, isUnique: Boolean)
    extends NamedDeclaration(name)

  case class FuncDecl(name: String, arguments: List[VarDecl], resultType: TypeExpr)
    extends NamedDeclaration(name)

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

    def ||(right: Expr) = FunctionCall("||", List(this, right))

    def ===(right: Expr) = FunctionCall("===", List(this, right))

    def <==>(right: Expr) = FunctionCall("<==>", List(this, right))

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

  def Exists(vars: VarDecl, expr: Expr): Forall = Forall(List(vars), expr)


  sealed abstract class Statement

  case class Block(stmts: List[Statement]) extends Statement

  def Block(stmts: Statement*): Block = Block(stmts.toList)

  case class Atomic(stmt: Statement) extends Statement

  case class LocalVar(name: String, typ: TypeExpr) extends Statement

  case class IfStmt(condition: Expr, ifTrue: Statement, ifFalse: Statement) extends Statement

  case class ProcCall(resultVar: Option[String], procname: String, arguments: List[Expr]) extends Statement

  case class Assignment(variable: String, expr: Expr) extends Statement

}
