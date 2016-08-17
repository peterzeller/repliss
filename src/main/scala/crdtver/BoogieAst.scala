package crdtver

object BoogieAst {


  sealed abstract class Element


  case class Program(declarations: List[Declaration])

  sealed abstract class Declaration extends Element


  case class TypeDecl(name: String) extends Declaration


  sealed abstract class NamedDeclaration(name: String) extends Element

  case class ConstantDecl(name: String, typ: TypeExpr, isUnique: Boolean)
    extends NamedDeclaration(name)

  case class FuncDecl(name: String, arguments: List[VarDecl], resultType: TypeExpr)
    extends NamedDeclaration(name)

  case class VarDecl(name: String, typ: TypeExpr)

  case class Axiom(expr: Expr) extends Declaration

  case class GlobalVariable(name: String, typ: TypeExpr)
    extends NamedDeclaration(name)


  case class Procedure(name: String,
                       arguments: List[VarDecl],
                       outArguments: List[VarDecl],
                       requires: List[Requires],
                       modifies: List[IdentifierExpr],
                       ensures: List[Ensures],
                       body: Statement)


  case class Requires(condition: Expr, isFree: Boolean)

  case class Ensures(condition: Expr, isFree: Boolean)



  sealed abstract class TypeExpr extends Element


  sealed abstract class Expr extends Element


  case class IdentifierExpr(name: String) extends Expr






  sealed abstract class Statement


}
