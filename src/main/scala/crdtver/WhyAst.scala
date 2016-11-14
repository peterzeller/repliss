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
  implicit case class UIdent(name: String) extends Ident(name)


  // identifier starting with lower case
  implicit case class LIdent(name: String) extends Ident(name)

  sealed abstract class Qualid(scope: List[UIdent], name: Ident)

  case class UQualid(scope: List[UIdent], name: UIdent) extends Qualid(scope, name)

  implicit def UQualid(name: String): UQualid = UQualid(List(), name)

  case class LQualid(scope: List[UIdent], name: LIdent) extends Qualid(scope, name)

  implicit def LQualid(name: String): LQualid = LQualid(List(), name)

  case class TQualid(scope: List[Ident], name: UIdent)


  case class File(theories: List[Theory]) extends Element


  case class Theory(
    name: UIdent,
    labels: List[Label],
    declarations: List[Declaration]
  ) extends Element

  case class Module(
    name: UIdent,
    labels: List[Label],
    declarations: List[MDecl]
  )

  sealed abstract class MDecl extends Element

  case class GlobalLet(
    isGhost: Boolean,
    name: LIdent,
    labels: List[Label],
    funBody: FunBody
  ) extends MDecl


  case class GlobalLetRec(
    recDefn: List[FunDefn]
  ) extends MDecl

  case class GlobalVariable(
    name: LIdent,
    typ: TypeExpression,
    isGhost: Boolean = false,
    labels: List[Label] = List()
  ) extends MDecl

  case class AbstractFunction(
    isGhost: Boolean,
    name: LIdent,
    labels: List[Label],
    params: List[Binder],
    returnType: TypeExpression,
    specs: List[Spec]
  ) extends MDecl

  case class ExceptionDecl(
    name: LIdent,
    labels: List[Label],
    typ: Option[TypeExpression]
  ) extends MDecl

  // TODO add namespace?

  sealed abstract class Declaration extends MDecl

  case class TypeDecls(
    decls: List[TypeDecl]
  ) extends Declaration

  case class ConstantDecl(
    name: LIdent,
    labels: List[Label],
    typ: TypeExpression,
    value: Option[Term]
  ) extends Declaration

  case class LogicDecls(
    decls: List[LogicDecl]
  ) extends Declaration

  case class InductiveDecls(
    isCoinductive: Boolean,
    decls: List[InductiveDecl]
  ) extends Declaration

  case class Axiom(
    name: Ident,
    formula: Term
  ) extends Declaration

  case class Lemma(
    name: Ident,
    formula: Term
  ) extends Declaration

  case class Goal(
    name: Ident,
    formula: Term
  ) extends Declaration

  case class Import(
    isClone: Boolean,
    impExp: ImpExp,
    name: TQualid,
    as: Option[UIdent],
    substitutions: List[SubstElt]

  ) extends Declaration

  case class Namespace(
    name: UIdent,
    declarations: List[Declaration]
  ) extends Declaration


  sealed abstract class SubstElt

  // TODO add SubstElt cases


  sealed abstract class ImpExp

  case class ImpExpImport() extends ImpExp

  case class ImpExpExport() extends ImpExp

  case class ImpExpNone() extends ImpExp


  case class TypeDecl(
    name: LIdent,
    typeParameters: List[TypeParamDecl] = List(),
    definition: TypeDefn,
    labels: List[Label] = List()
  )

  sealed abstract class TypeDefn

  case class AbstractType() extends TypeDefn

  case class AliasType(
    alias: TypeExpression
  ) extends TypeDefn

  case class AlgebraicType(
    cases: List[TypeCase],
    invariants: List[Invariant] = List()
  ) extends TypeDefn

  case class TypeCase(
    name: UIdent,
    paramsTypes: List[TypeParam] = List(),
    labels: List[Label] = List()
  )

  case class RecordType(
    fields: List[RecordField],
    invariants: List[Invariant]
  ) extends TypeDefn

  case class RecordField(
    name: LIdent,
    labels: List[Label],
    typ: TypeExpression,
    isGhost: Boolean,
    isMutable: Boolean
  ) extends TypeDefn


  // TOdO


  case class LogicDecl(
    name: LIdent,
    labels: List[Label],
    typeParams: List[TypeParam],
    typ: TypeExpression,
    implementation: Term
  )

  case class TypeParamDecl(
    name: LIdent,
    labels: List[Label]
  )

  case class TypeParam(
    name: LIdent,
    typ: TypeExpression
  )


  // TODO


  case class InductiveDecl(
    name: LIdent,
    labels: List[Label],
    typeParams: List[TypeParam],
    cases: List[InductiveCase]
  )

  case class InductiveCase(
    name: Ident,
    labels: List[Label],
    formula: Term
  )


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

  case class LambdaAbstraction(
    params: List[Binder],
    specs: List[Spec],
    otherSpecs: List[Spec],
    body: Term
  )

  case class FunBody(
    params: List[Binder],
    returnType: Option[TypeExpression],
    specs: List[Spec],
    otherSpecs: List[Spec],
    body: Term
  )

  case class Binder(
    isGhost: Boolean,
    name: LIdent,
    labels: List[Label],
    typ: TypeExpression
  )

  case class LetTerm(
    pattern: Pattern,
    value: Term,
    body: Term
  ) extends Term

  case class LetRec(
    definitions: List[FunDefn],
    body: Term
  )

  case class FunDefn(
    isGhost: Boolean,
    name: LIdent,
    labels: List[Label],
    body: FunBody
  )

  case class Sequence(
    terms: List[Term]
  ) extends Term


  case class Loop(
    invariants: List[Invariant],
    variant: Option[Variant],
    body: Term
  ) extends Term

  case class While(
    condition: Term,
    invariants: List[Invariant],
    variant: Option[Variant],
    body: Term
  ) extends Term

  // TODO for-loop

  // TODO raise exceptions

  // TODO blackbox


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

  case class FieldAssignment(
    recordTerm: Term,
    fieldName: LQualid,
    newValue: Term
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

  case class Old(
    term: Term
  ) extends Term

  case class At(
    term: Term,
    programPoint: UIdent
  )

  // Program expressions (Fig 7.7):


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


  // Figure 7.6: Specification clauses in programs

  sealed abstract class Spec

  case class Requires(
    formula: Term
  ) extends Spec

  case class Ensures(
    formula: Term
  ) extends Spec

  case class Returns(
    cases: List[FormulaCase]
  ) extends Spec

  case class FormulaCase(
    pattern: Pattern,
    formula: Term
  )

  case class Reads(
    terms: List[Term]
  ) extends Spec

  case class Writes(
    terms: List[Term]
  ) extends Spec


  case class RaisesName(
    raised: List[UQualid]
  ) extends Spec

  case class Raises(
    cases: List[RaisesCase]
  ) extends Spec

  case class RaisesCase(
    name: UQualid,
    pattern: Option[Pattern],
    formula: Term
  )

  case class Variant(
    variants: List[OneVariant]
  ) extends Spec

  case class OneVariant(
    term: Term,
    variantRel: Option[LQualid]
  )

  case class Invariant(
    formula: Term
  )

  sealed abstract class Assertion(formula: Term)

  case class Assert(
    formula: Term
  ) extends Assertion(formula)

  case class Assume(
    formula: Term
  ) extends Assertion(formula)

  case class Check(
    formula: Term
  ) extends Assertion(formula)


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
