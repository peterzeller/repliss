package crdtver

object WhyAst {


  sealed abstract class TraceInfo

  case class AstElementTraceInfo(source: InputAst.AstElem) extends TraceInfo

  case class EndAtomicTraceInfo(source: InputAst.AstElem) extends TraceInfo

  case class TextTraceInfo(text: String) extends TraceInfo

  sealed abstract class Element {
    var trace: TraceInfo = _


    def setTrace(trace: TraceInfo): this.type = {
      this.trace = trace
      this
    }

  }


  sealed abstract class Ident(name: String)

  // identifier starting with upper case
  case class UIdent(name: String) extends Ident(name)

  // identifier starting with lower case
  case class LIdent(name: String) extends Ident(name)

  sealed abstract class Qualid(scope: List[UIdent], name: Ident)

  case class UQualid(scope: List[UIdent], name: UIdent) extends Qualid(scope, name)

  case class LQualid(scope: List[UIdent], name: LIdent) extends Qualid(scope, name)


  case class File(theories: List[Theory]) extends Element


  case class Theory(
    name: UIdent,
    labels: List[Label],
    declarations: List[Declaration]
  ) extends Element

  sealed abstract class Declaration extends Element

  case class TypeDecls(
    decls: List[TypeDecl]
  ) extends Declaration

  case class ConstantDecl(
    name: LIdent,
    labels: List[Label],
    typ: TypeExpression,
    value: Option[Term]
  ) extends Declaration





  sealed abstract class TypeDecl

  // TOdO


  sealed abstract class TypeExpression extends Element


  case class TypeSymbol(
    name: LQualid,
    typeArgs: List[TypeExpression] = List()
  ) extends TypeExpression

  case class TypeVariable(
    name: LIdent
  ) extends TypeExpression

  case class TupleType(
    types: List[TypeExpression]
  )


  sealed abstract class Term extends Element

  // TODO add elements from formulas (page 80)

  case class IntegerConstant(value: BigInt) extends Term

  case class RealConstant(value: BigDecimal) extends Term

  case class Symbol(name: String) extends Term

  case class FunctionCall(
    funcName: LQualid,
    args: List[Term]
  ) extends Term

  case class ArrayLookup(
    arrayTerm: Term,
    indexTerm: Term
  ) extends Term

  case class ArrayUpdate(
    arrayTerm: Term,
    indexTerm: Term,
    newValue: Term
  ) extends Term

  case class Conditional(
    condition: Term,
    ifTrue: Term,
    ifFalse: Term
  ) extends Term

  case class LetTerm(
    pattern: Pattern,
    value: Term,
    body: Term
  ) extends Term

  case class MatchTerm(
    terms: List[Term],
    cases: List[TermCase]
  ) extends Term

  case class TermCase(
    pattern: Pattern,
    term: Term
  ) extends Element

  case class Tuple(
    values: List[Term]
  ) extends Term

  case class RecordTerm(
    fields: List[TermField]
  ) extends Term

  case class TermField(
    fieldName: LQualid,
    term: Term
  ) extends Element

  case class FieldAccess(
    recordTerm: Term,
    fieldName: LQualid
  ) extends Term

  case class FieldUpdate(
    recordTerm: Term,
    fieldUpdates: List[TermField]
  ) extends Term

  case class CastTerm(
    term: Term,
    typ: TypeExpression
  ) extends Term

  case class LabeledTerm(
    label: Label,
    term: Term
  ) extends Term

  case class Label()

  case class CodeMark(
    name: UIdent
  ) extends Term


  sealed abstract class Pattern

  case class OrPattern(
    patterns: List[Pattern]
  ) extends Pattern

  case class TuplePattern(
    patterns: List[Pattern]
  ) extends Pattern

  case class CatchAllPattern() extends Pattern

  case class VariablePattern(
    name: LIdent
  ) extends Pattern

  case class ConstructorPattern(
    constructorName: UIdent,
    args: List[Pattern]
  ) extends Pattern

  case class BindingPattern(
    pattern: Pattern,
    name: LIdent
  )


  // TODO


  // old stuff:
  /*








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


  sealed abstract class Statement extends Element

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

  case class NondetIf(alternatives: List[Statement]) extends Statement

  case class ProcCall(resultVar: Option[String], procname: String, arguments: List[Expr]) extends Statement

  case class Assignment(variable: String, expr: Expr) extends Statement

  case class Havoc(variable: String) extends Statement

  case class Return(expr: Expr) extends Statement

  case class Assert(expr: Expr, attributes: List[Attribute] = List()) extends Statement

  case class Assume(expr: Expr, attributes: List[Attribute] = List()) extends Statement
*/
}
