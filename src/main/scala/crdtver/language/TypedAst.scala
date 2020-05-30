package crdtver.language

import crdtver.language.ACrdtInstance.StructInstance
import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.NoSource
import crdtver.parser.LangParser._
import crdtver.testing.Interpreter.AnyValue
import crdtver.utils.PrettyPrintDoc._
import crdtver.utils.myMemo

import scala.language.implicitConversions

/**
  * This defines the abstract syntax of the Repliss input language in terms of case classes.
  */
object TypedAst {

  sealed abstract class AstElem(source: SourceTrace) {

    def getSource(): SourceTrace = source

    override def toString: String = customToString.prettyStr(120)

    def customToString: Doc

    def printAst = TypedAstPrinter.print(this)
  }

  case class InProgram(
    name: String,
    source: ProgramContext,
    procedures: List[InProcedure],
    types: List[InTypeDecl],
    axioms: List[InAxiomDecl],
    invariants: List[InInvariantDecl],
    programCrdt: ACrdtInstance = StructInstance(fields = Map())
  ) extends AstElem(source) {

    private val queryCache = new myMemo[String, Option[InQueryDecl]]({ name: String =>
      programCrdt.queryDefinitions().find(_.name.name == name)
    })

    def findQuery(name: String): Option[InQueryDecl] =
      queryCache(name)

    def hasQuery(name: String): Boolean =
      queryCache(name).isDefined

    override def customToString: Doc = "program"


    def findProcedure(procname: String): InProcedure =
      tryFindProcedure(procname)
        .getOrElse(throw new RuntimeException(s"Procedure $procname not found."))

    def tryFindProcedure(procname: String): Option[InProcedure] = {
      procedures.find(p => p.name.name == procname)
    }

    def findType(name: String): Option[InTypeDecl] =
      types.find(t => t.name.name == name)

    def findDatatype(name: String): Option[InTypeDecl] =
      findType(name).find(t => t.dataTypeCases.nonEmpty)

  }

  sealed abstract class InDeclaration(source: SourceTrace) extends AstElem(source: SourceTrace) {

  }

  case class InProcedure(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable],
    locals: List[InVariable],
    returnType: InTypeExpr,
    body: InStatement
  ) extends InDeclaration(source) {
    override def customToString: Doc = s"procedure $name"
  }

  case class InTypeDecl(
    source: SourceTrace,
    isIdType: Boolean,
    name: Identifier,
    dataTypeCases: List[DataTypeCase]
  ) extends InDeclaration(source) {
    override def customToString: Doc = s"type $name"

  }

  case class DataTypeCase(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable]
  ) extends AstElem(source) {
    override def customToString: Doc = s"datatype case $name"
  }


  case class InOperationDecl(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable]
  ) extends InDeclaration(source) {
    override def customToString: Doc = s"operation $name"
  }

  case class InQueryDecl(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable],
    returnType: InTypeExpr,
    implementation: Option[InExpr],
    ensures: Option[InExpr],
    annotations: Set[InAnnotation]
  ) extends InDeclaration(source) {
    override def customToString: Doc = s"query $name"
  }

  type InAnnotation = InputAst.InAnnotation


  case class InAxiomDecl(
    source: SourceTrace,
    expr: InExpr
  ) extends InDeclaration(source) {
    override def customToString: Doc = s"axiom $expr"
  }

  case class InInvariantDecl(
    source: SourceTrace,
    name: String,
    isFree: Boolean,
    expr: InExpr
  ) extends InDeclaration(source) {
    override def customToString: Doc = nested(4, s"invariant" <+> name <> ":" </> expr.customToString)
  }

  case class InCrdtDecl(
    source: SourceTrace,
    keyDecl: InKeyDecl
  ) extends InDeclaration(source) {
    override def customToString: Doc = s"crdt $keyDecl"
  }

  case class InKeyDecl(
    source: SourceTrace,
    name: Identifier,
    crdttype: InCrdtType)
    extends AstElem(source) {
    override def customToString: Doc = s"crdttype $crdttype"
  }

  sealed abstract class InCrdtType(source: SourceTrace)
    extends AstElem(source: SourceTrace) {
  }

  case class InCrdt(
    source: SourceTrace,
    name: Identifier,
    typ: List[InCrdtType]
  ) extends InCrdtType(source) {
    override def customToString: Doc = s"typ $name $typ"
  }

  case class InStructCrdt(
    source: SourceTrace,
    keyDecl: List[InKeyDecl]
  ) extends InCrdtType(source) {
    override def customToString: Doc = s"key $keyDecl"
  }

  type SourceRange = InputAst.SourceRange

  type SourcePosition = InputAst.SourcePosition

  type SourceTrace = InputAst.SourceTrace
  type Identifier = InputAst.Identifier

  case class InVariable(
    source: SourceTrace,
    name: Identifier,
    typ: InTypeExpr)
    extends AstElem(source) {

    def subst(subst: Map[TypeVarUse, InTypeExpr]): InVariable =
      copy(typ = typ.subst(subst))

    override def customToString: Doc = s"var $name: $typ"
  }

  sealed abstract class InExpr(source: SourceTrace, typ: InTypeExpr)
    extends AstElem(source: SourceTrace) {
    def getTyp: InTypeExpr = typ
  }

  case class VarUse(
    source: SourceTrace,
    typ: InTypeExpr,
    name: String
  ) extends InExpr(source, typ) {
    override def customToString: Doc = name
  }

  case class BoolConst(
    source: SourceTrace,
    typ: InTypeExpr,
    value: Boolean
  ) extends InExpr(source, typ) {
    override def customToString: Doc = value.toString
  }

  case class IntConst(
    source: SourceTrace,
    typ: InTypeExpr,
    value: BigInt
  ) extends InExpr(source, typ) {
    override def customToString: Doc = value.toString
  }


  //  case class FieldAccess(
  //    source: SourceTrace,
  //    typ: InTypeExpr,
  //    receiver: InExpr,
  //    fieldName: Identifier
  //  ) extends InExpr(source, typ)


  sealed abstract class CallExpr(
    source: SourceTrace,
    typ: InTypeExpr,
    args: List[InExpr]
  ) extends InExpr(source, typ)

  case class FunctionCall(
    source: SourceTrace,
    typ: InTypeExpr,
    functionName: Identifier,
    args: List[InExpr],
    kind: FunctionKind
  ) extends CallExpr(source, typ, args) {
    override def customToString: Doc = s"$functionName(${args.mkString(", ")})"
  }


  case class ApplyBuiltin(
    source: SourceTrace,
    typ: InTypeExpr,
    function: BuiltInFunc,
    args: List[InExpr]
  ) extends CallExpr(source, typ, args) {

    // preconditions:
    function match {
      case BF_happensBefore(HappensBeforeOn.Unknown()) =>
        throw new IllegalArgumentException("Happens before must not be unknown")
      case _ =>
    }

    override def customToString: Doc = {
      function match {
        case BF_isVisible() => args.head.customToString <> " is visible"
        case BF_happensBefore(_) => 
          operator(args.head.customToString, "happens before", args(1).customToString)
        case BF_sameTransaction() => functionCall("sameTransaction", args(0).customToString, args(1).customToString)
        case BF_less() => operator(args(0).customToString, "<", args(1).customToString)
        case BF_lessEq() => operator(args(0).customToString, "<=", args(1).customToString)
        case BF_greater() => operator(args(0).customToString, ">", args(1).customToString)
        case BF_greaterEq() => operator(args(0).customToString, ">=", args(1).customToString)
        case BF_equals() => operator(args(0).customToString, "==", args(1).customToString)
        case BF_notEquals() => operator(args(0).customToString, "!=", args(1).customToString)
        case BF_and() => operator(args(0).customToString, "&&", args(1).customToString)
        case BF_or() => operator(args(0).customToString, "||", args(1).customToString)
        case BF_implies() => operator(args(0).customToString, "==>", args(1).customToString)

        case BF_plus() => operator(args(0).customToString, "+", args(1).customToString)
        case BF_minus() => operator(args(0).customToString, "-", args(1).customToString)
        case BF_mult() => operator(args(0).customToString, "*", args(1).customToString)
        case BF_div() => operator(args(0).customToString, "/", args(1).customToString)
        case BF_mod() => operator(args(0).customToString, "%", args(1).customToString)
        case BF_not() => functionCall("!", args.head.customToString)
        case BF_getOperation() => args.head.customToString <> ".op"
        case BF_getInfo() => args.head.customToString <> ".info"
        case BF_getResult() => args.head.customToString <> ".result"
        case BF_getOrigin() => args.head.customToString <> ".origin"
        case BF_getTransaction() => args.head.customToString <> ".transaction"
        case BF_inCurrentInvoc() => args.head.customToString <> ".inCurrentInvoc"
        case BF_distinct() => functionCall("distinct", args.map(_.customToString))
      }
    }
  }

  case class QuantifierExpr(
    source: SourceTrace,
    typ: InTypeExpr,
    quantifier: Quantifier,
    vars: List[InVariable],
    expr: InExpr
  ) extends InExpr(source, typ) {
    override def customToString: Doc = group(nested(4,
      "(" <> quantifier.toString <+> sep(", ", vars.map(_.customToString)) <+> "::" </> expr.customToString <> ")"))
  }

  case class InAllValidSnapshots(expr: InExpr) extends InExpr(expr.getSource(), expr.getTyp) {
    override def customToString: Doc = s"(in all valid snapshots :: $expr)"
  }


  type Quantifier = InputAst.Quantifier

  type BuiltInFunc = InputAst.BuiltInFunc


  sealed abstract class InStatement(source: SourceTrace)
    extends AstElem(source: SourceTrace) {

  }

  case class BlockStmt(
    source: SourceTrace,
    stmts: List[InStatement]
  ) extends InStatement(source) {
    override def customToString: Doc = s"{${stmts.mkString(";")}}"
  }

  def makeBlock(
    source: SourceTrace,
    stmts: List[InStatement]
  ): BlockStmt = BlockStmt(
    source,
    stmts.flatMap(flatten)
  )

  private def flatten(s: InStatement): List[InStatement] = s match {
    case BlockStmt(source, stmts) => stmts.flatMap(flatten)
    case _ => List(s)
  }

  case class Atomic(
    source: SourceTrace,
    body: InStatement
  ) extends InStatement(source) {
    override def customToString: Doc = s"atomic $body"
  }


  case class LocalVar(
    source: SourceTrace,
    variable: InVariable
  ) extends InStatement(source) {
    override def customToString: Doc = s"var $variable"
  }


  case class IfStmt(
    source: SourceTrace,
    cond: InExpr,
    thenStmt: InStatement,
    elseStmt: InStatement
  ) extends InStatement(source) {
    override def customToString: Doc = s"if ($cond) $thenStmt else $elseStmt"
  }

  case class MatchStmt(
    source: SourceTrace,
    expr: InExpr,
    cases: List[MatchCase]
  ) extends InStatement(source) {
    override def customToString: Doc = s"$expr match { ${cases.mkString(";")} }"
  }

  case class MatchCase(
    source: SourceTrace,
    pattern: InExpr,
    statement: InStatement
  ) extends AstElem(source) {

    override def customToString: Doc = s"case $pattern => $statement"
  }


  case class CrdtCall(
    source: SourceTrace,
    call: FunctionCall
  ) extends InStatement(source) {
    override def customToString: Doc = s"call $call"
  }


  case class Assignment(
    source: SourceTrace,
    varname: Identifier,
    expr: InExpr
  ) extends InStatement(source) {
    override def customToString: Doc = s"$varname := $expr"
  }


  case class NewIdStmt(
    source: SourceTrace,
    varname: Identifier,
    typename: IdType
  ) extends InStatement(source) {
    override def customToString: Doc = s"$varname := new $typename"
  }


  case class ReturnStmt(
    source: SourceTrace,
    expr: InExpr,
    assertions: List[AssertStmt]
  ) extends InStatement(source) {
    override def customToString: Doc = s"return $expr"
  }


  case class AssertStmt(
    source: SourceTrace,
    expr: InExpr
  ) extends InStatement(source) {
    override def customToString: Doc = s"assert $expr"
  }


  sealed abstract class InTypeExpr(source: SourceTrace = NoSource())
    extends AstElem(source: SourceTrace) {

    def freeVars: Set[TypeVarUse] = this match {
      case AnyType() => Set()
      case BoolType() => Set()
      case IntType() => Set()
      case CallIdType() => Set()
      case InvocationIdType() => Set()
      case TransactionIdType() => Set()
      case InvocationInfoType() => Set()
      case InvocationResultType() => Set()
      case SomeOperationType() => Set()
      case OperationType(name) => Set()
      case TypedAst.FunctionType(argTypes, returnType, functionKind) =>
        (argTypes.view.flatMap(_.freeVars) ++ returnType.freeVars).toSet
      case SimpleType(name, typeArgs) =>
        typeArgs.view.flatMap(_.freeVars).toSet
      case v: TypeVarUse => Set(v)
      case IdType(name) => Set()
    }

    def subst(s: Map[TypeVarUse, InTypeExpr]): InTypeExpr = this match {
      case TypedAst.FunctionType(argTypes, returnType, functionKind) =>
        TypedAst.FunctionType(argTypes.map(_.subst(s)), returnType.subst(s), functionKind)()
      case SimpleType(name, typeArgs) =>
        SimpleType(name, typeArgs.map(_.subst(s)))()
      case v: TypeVarUse =>
        s.get(v) match {
          case Some(t) => t
          case None => this
        }
      case _ => this
    }


  }

  case class AnyType() extends InTypeExpr {

    override def customToString: Doc = "any"
  }

  case class UnitType() extends InTypeExpr {

    override def customToString: Doc = "Unit"
  }

  case class BoolType() extends InTypeExpr {

    override def customToString: Doc = "bool"
  }

  case class IntType() extends InTypeExpr {

    override def customToString: Doc = "int"
  }

  case class CallIdType() extends InTypeExpr {

    override def customToString: Doc = "callId"
  }

  case class InvocationIdType() extends InTypeExpr {

    override def customToString: Doc = "invocationId"
  }


  case class TransactionIdType() extends InTypeExpr {

    override def customToString: Doc = "transactionId"
  }

  case class InvocationInfoType() extends InTypeExpr {

    override def customToString: Doc = "invocationInfo"
  }

  case class InvocationResultType() extends InTypeExpr {

    override def customToString: Doc = "invocationResult"
  }

  case class SomeOperationType() extends InTypeExpr {

    override def customToString: Doc = "operation"
  }

  case class OperationType(name: String)(source: SourceTrace = NoSource())
    extends InTypeExpr(source) {


    override def customToString: Doc = s"operation<$name>"
  }

  sealed abstract class FunctionKind

  object FunctionKind {

    case class FunctionKindDatatypeConstructor() extends FunctionKind

    case class FunctionKindCrdtQuery() extends FunctionKind

  }

  /**
   * Polymorphic type for functions
   */
  case class PrincipleType(
    typeParams: List[TypeVarUse],
    typ: InTypeExpr
  ) {

  }

  case class FunctionType(argTypes: List[InTypeExpr], returnType: InTypeExpr, functionKind: FunctionKind)(source: SourceTrace = NoSource())
    extends InTypeExpr(source) {


    override def customToString: Doc = s"(${argTypes.mkString(", ")}) => $returnType"
  }

  case class SimpleType(name: String, typeArgs: List[InTypeExpr])(source: SourceTrace = NoSource()) extends InTypeExpr(source) {

    override def customToString: Doc = name
  }

  case class TypeVarUse(name: String)(source: SourceTrace = NoSource()) extends InTypeExpr(source) {

    override def customToString: Doc = name
  }

  case class IdType(name: String)(source: SourceTrace = NoSource()) extends InTypeExpr(source) {
    override def customToString: Doc = name
  }


  def extractIds(result: AnyValue, returnType: Option[InTypeExpr]): Map[IdType, Set[AnyValue]] = returnType match {
    case Some(t) =>
      t match {
        case idt@IdType(name) =>
          Map(idt -> Set(result))
        case _ =>
          // TODO handle datatypes with nested ids
          Map()
      }
    case None =>
      Map()
  }

}
