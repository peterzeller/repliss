package crdtver.language

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.NoSource
import crdtver.language.TypedAst.FunctionKind.{FunctionKindCrdtQuery, FunctionKindDatatypeConstructor}
import crdtver.language.crdts.CrdtTypeDefinition.Operation
import crdtver.language.crdts.{CrdtInstance, CrdtTypeDefinition, UniqueName}
import crdtver.parser.LangParser._
import crdtver.testing.Interpreter.AnyValue

import scala.language.implicitConversions

/**
  * This defines the abstract syntax of the Repliss input language in terms of case classes.
  */
object TypedAst {

  sealed abstract class AstElem(source: SourceTrace) {
    def getErrorSource(): SourceTrace = getSource()


    def getSource(): SourceTrace = source

    override def toString: String = customToString

    def customToString: String

    def printAst = TypedAstPrinter.print(this)
  }

  sealed trait Definition {
    def name: UniqueName

    def typ: InTypeExpr

  }

  /** in case no definition was found */
  case class NoDefinition(name: UniqueName) extends Definition {
    override def typ: InTypeExpr = AnyType()
  }

  case class CrdtDefinition(name: UniqueName, instance: CrdtInstance) extends Definition {
    override def typ: InTypeExpr = {
      val operations = instance.operations
      TypedAst.FunctionType(List(NestedOperationType(operations)),
        DependentReturnType(operations), FunctionKindCrdtQuery())()
    }
  }

  case class BuiltinDefinition(name: UniqueName, typ: InTypeExpr) extends Definition

  case class InProgram(
    name: String,
    source: ProgramContext,
    procedures: List[InProcedure],
    types: List[InTypeDecl],
    axioms: List[InAxiomDecl],
    invariants: List[InInvariantDecl],
    programCrdt: CrdtInstance = StructInstance(fields = Map(), crdtContext = new crdts.NameContext())
  ) extends AstElem(source) {

    override def customToString: String = "program"


    def findProcedure(procname: String): InProcedure =
      procedures.find(p => p.name.name == procname)
        .getOrElse(throw new RuntimeException(s"Procedure $procname not found."))

    def findType(name: String): Option[InTypeDecl] =
      types.find(t => t.name.name == name)

    def findType(name: UniqueName): Option[InTypeDecl] =
      types.find(t => t.name == name)

    def findDatatype(name: String): Option[InTypeDecl] =
      findType(name).find(t => t.dataTypeCases.nonEmpty)

    def findDatatype(name: UniqueName): Option[InTypeDecl] =
      findType(name).find(t => t.dataTypeCases.nonEmpty)


  }

  sealed abstract class InDeclaration(source: SourceTrace) extends AstElem(source: SourceTrace) {

  }

  case class InProcedure(
    source: SourceTrace,
    name: UniqueName,
    params: List[InVariable],
    locals: List[InVariable],
    returnType: Option[InTypeExpr],
    body: InStatement
  ) extends InDeclaration(source) {
    override def customToString: String = s"procedure $name"
  }

  case class InTypeDecl(
    source: SourceTrace,
    isIdType: Boolean,
    name: UniqueName,
    dataTypeCases: List[DataTypeCase]
  ) extends InDeclaration(source) {
    def findDatatypeCase(name: String): Option[DataTypeCase] =
      dataTypeCases.find(_.name.name == name)

    override def customToString: String = s"type $name"

  }

  case class DataTypeCase(
    source: SourceTrace,
    name: UniqueName,
    params: List[InVariable],
    returnTyp: InTypeExpr
  ) extends AstElem(source) with Definition {

    override def customToString: String = s"datatype case $name"

    override def typ: FunctionType = functionType(params.map(_.typ), returnTyp, FunctionKindDatatypeConstructor())()
  }


  @deprecated("Operations are now part of the program CRDT")
  case class InOperationDecl(
    source: SourceTrace,
    name: UniqueName,
    params: List[InVariable]
  ) extends InDeclaration(source) {
    override def customToString: String = s"operation $name"
  }

  @deprecated("Queries are now part of the program CRDT")
  case class InQueryDecl(
    source: SourceTrace,
    name: UniqueName,
    params: List[InVariable],
    returnType: InTypeExpr,
    implementation: Option[InExpr],
    ensures: Option[InExpr],
    annotations: Set[InAnnotation]
  ) extends InDeclaration(source) {
    override def customToString: String = s"query $name"
  }

  type InAnnotation = InputAst.InAnnotation


  case class InAxiomDecl(
    source: SourceTrace,
    expr: InExpr
  ) extends InDeclaration(source) {
    override def customToString: String = s"axiom $expr"
  }

  case class InInvariantDecl(
    source: SourceTrace,
    expr: InExpr
  ) extends InDeclaration(source) {
    override def customToString: String = s"invariant $expr"
  }

  case class InCrdtDecl(
    source: SourceTrace,
    keyDecl: InKeyDecl
  ) extends InDeclaration(source) {
    override def customToString: String = s"crdt $keyDecl"
  }

  case class InKeyDecl(
    source: SourceTrace,
    name: UniqueName,
    crdttype: InCrdtType)
    extends AstElem(source) {
    override def customToString: String = s"crdttype $crdttype"
  }

  sealed abstract class InCrdtType(source: SourceTrace)
    extends AstElem(source: SourceTrace) {
  }

  case class InCrdt(
    source: SourceTrace,
    name: UniqueName,
    typ: List[InCrdtType]
  ) extends InCrdtType(source) {
    override def customToString: String = s"typ $name $typ"
  }

  case class InStructCrdt(
    source: SourceTrace,
    keyDecl: List[InKeyDecl]
  ) extends InCrdtType(source) {
    override def customToString: String = s"key $keyDecl"
  }

  type SourceRange = InputAst.SourceRange

  type SourcePosition = InputAst.SourcePosition

  type SourceTrace = InputAst.SourceTrace

  case class InVariable(
    source: SourceTrace,
    name: UniqueName,
    typ: InTypeExpr)
    extends AstElem(source) with Definition {
    override def customToString: String = s"var $name: $typ"
  }

  sealed abstract class InExpr(source: SourceTrace, typ: InTypeExpr)
    extends AstElem(source: SourceTrace) {
    def withType(t: InTypeExpr): InExpr

    def getTyp: InTypeExpr = typ
  }

  case class VarUse(
    source: SourceTrace,
    typ: InTypeExpr,
    name: UniqueName
  ) extends InExpr(source, typ) {
    override def customToString: String = name.toString

    override def withType(t: InTypeExpr): InExpr =
      this.copy(typ = t)
  }

  case class BoolConst(
    source: SourceTrace,
    typ: InTypeExpr,
    value: Boolean
  ) extends InExpr(source, typ) {
    override def customToString: String = value.toString

    override def withType(t: InTypeExpr): InExpr =
      this.copy(typ = t)
  }

  case class IntConst(
    source: SourceTrace,
    typ: InTypeExpr,
    value: BigInt
  ) extends InExpr(source, typ) {
    override def customToString: String = value.toString

    override def withType(t: InTypeExpr): InExpr =
      this.copy(typ = t)
  }


  //  case class FieldAccess(
  //    source: SourceTrace,
  //    typ: InTypeExpr,
  //    receiver: InExpr,
  //    fieldName: UniqueName
  //  ) extends InExpr(source, typ)


  sealed abstract class CallExpr(
    source: SourceTrace,
    typ: InTypeExpr,
    args: List[InExpr]
  ) extends InExpr(source, typ)

  case class FunctionCall(
    source: SourceTrace,
    typ: InTypeExpr,
    functionName: UniqueName,
    args: List[InExpr],
    kind: FunctionKind
  ) extends CallExpr(source, typ, args) {
    override def customToString: String = s"$functionName(${args.mkString(", ")})"

    override def withType(t: InTypeExpr): InExpr =
      this.copy(typ = t)
  }


  case class DatabaseCall(
    source: SourceTrace,
    typ: InTypeExpr,
    crdtInstance: CrdtInstance,
    operation: FunctionCall
  ) extends CallExpr(source, typ, List(operation)) {
    override def customToString: String = s"<$crdtInstance>($operation})"

    override def withType(t: InTypeExpr): InExpr =
      this.copy(typ = t)
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

    override def withType(t: InTypeExpr): InExpr =
      this.copy(typ = t)

    override def customToString: String = {
      function match {
        case BF_isVisible() => s"${args.head} is visible"
        case BF_happensBefore(_) => s"(${args.head} happens before ${args(1)})"
        case BF_sameTransaction() => s"sameTransaction(${args(0)}, ${args(1)})"
        case BF_less() => s"(${args.head} < ${args(1)})"
        case BF_lessEq() => s"(${args.head} <= ${args(1)})"
        case BF_greater() => s"(${args.head} > ${args(1)})"
        case BF_greaterEq() => s"(${args.head} >= ${args(1)})"
        case BF_equals() => s"(${args.head} == ${args(1)})"
        case BF_notEquals() => s"(${args.head} != ${args(1)})"
        case BF_and() => s"(${args.head} && ${args(1)})"
        case BF_or() => s"(${args.head} || ${args(1)})"
        case BF_implies() => s"(${args.head} ==> ${args(1)})"

        case BF_plus() => s"(${args.head} + ${args(1)})"
        case BF_minus() => s"(${args.head} - ${args(1)})"
        case BF_mult() => s"(${args.head} * ${args(1)})"
        case BF_div() => s"(${args.head} / ${args(1)})"
        case BF_mod() => s"(${args.head} % ${args(1)})"
        case BF_not() => s"!(${args.head})"
        case BF_getOperation() => s"${args.head}.op"
        case BF_getInfo() => s"${args.head}.info"
        case BF_getResult() => s"${args.head}.result"
        case BF_getOrigin() => s"${args.head}.origin"
        case BF_getTransaction() => s"{args.head}.transaction"
        case BF_inCurrentInvoc() => s"${args.head}.inCurrentInvoc"
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
    override def customToString: String = s"($quantifier ${vars.mkString(", ")} :: $expr) "

    override def withType(t: InTypeExpr): InExpr =
      this.copy(typ = t)
  }

  case class InAllValidSnapshots(expr: InExpr) extends InExpr(expr.getSource(), expr.getTyp) {
    override def customToString: String = s"(in all valid snapshots :: $expr)"

    override def withType(t: InTypeExpr): InExpr = this
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
    override def customToString: String = s"{${stmts.mkString(";")}}"
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
    override def customToString: String = s"atomic $body"
  }


  case class LocalVar(
    source: SourceTrace,
    variable: InVariable
  ) extends InStatement(source) {
    override def customToString: String = s"var $variable"
  }


  case class IfStmt(
    source: SourceTrace,
    cond: InExpr,
    thenStmt: InStatement,
    elseStmt: InStatement
  ) extends InStatement(source) {
    override def customToString: String = s"if ($cond) $thenStmt else $elseStmt"
  }

  case class MatchStmt(
    source: SourceTrace,
    expr: InExpr,
    cases: List[MatchCase]
  ) extends InStatement(source) {
    override def customToString: String = s"$expr match { ${cases.mkString(";")} }"
  }

  case class MatchCase(
    source: SourceTrace,
    pattern: InPattern,
    statement: InStatement
  ) extends AstElem(source) {

    override def customToString: String = s"case $pattern => $statement"
  }


  sealed abstract class InPattern(source: SourceTrace) extends AstElem(source)

  case class InPatternApply(
    source: SourceTrace,
    name: UniqueName,
    args: List[InPattern]) extends InPattern(source) {
    override def customToString: String = s"$name(${args.map(_.toString).mkString(", ")})"
  }

  case class InPatternVar(
    source: SourceTrace,
    name: UniqueName,
    typ: InTypeExpr
  ) extends InPattern(source) with Definition {
    override def customToString: String = name.toString
  }

  case class CrdtCall(
    source: SourceTrace,
    result: Option[UniqueName],
    crdtInstance: CrdtInstance,
    operation: FunctionCall
  ) extends InStatement(source) {
    override def customToString: String = s"call <$crdtInstance>.$operation"
  }


  case class Assignment(
    source: SourceTrace,
    varname: UniqueName,
    expr: InExpr
  ) extends InStatement(source) {
    override def customToString: String = s"$varname := $expr"
  }


  case class NewIdStmt(
    source: SourceTrace,
    varname: UniqueName,
    typename: InTypeExpr
  ) extends InStatement(source) {
    override def customToString: String = s"$varname := new $typename"
  }


  case class ReturnStmt(
    source: SourceTrace,
    expr: InExpr,
    assertions: List[AssertStmt]
  ) extends InStatement(source) {
    override def customToString: String = s"return $expr"
  }


  case class AssertStmt(
    source: SourceTrace,
    expr: InExpr
  ) extends InStatement(source) {
    override def customToString: String = s"assert $expr"
  }


  sealed abstract class InTypeExpr(source: SourceTrace = NoSource())
    extends AstElem(source: SourceTrace) {
    def isSubtypeOfIntern(other: InTypeExpr): Boolean


    def isSubtypeOf(other: InTypeExpr): Boolean = {
      other.isInstanceOf[AnyType] || this.isSubtypeOfIntern(other)
    }

    def equalsType(other: InTypeExpr): Boolean = {
      (this isSubtypeOf other) && (other isSubtypeOf this)
    }

  }

  case class AnyType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = true

    override def customToString: String = "any"
  }

  case class TypeUnit() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean =
      this == other

    override def customToString: String = "Unit"
  }

  /** return type = operation return type of last argument */
  case class DependentReturnType(operations: List[Operation]) extends InTypeExpr {
    // TODO list of operations really needed?

    override def isSubtypeOfIntern(other: InTypeExpr): Boolean =
      this == other

    override def customToString: String = "dependent-return-type"
  }

  case class BoolType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "bool"
  }

  case class IntType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "int"
  }

  case class CallIdType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "callId"
  }

  case class InvocationIdType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "invocationId"
  }


  case class TransactionIdType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "transactionId"
  }

  case class InvocationInfoType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "invocationInfo"
  }

  case class InvocationResultType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "invocationResult"
  }

  case class SomeOperationType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other == this

    override def customToString: String = "operation"
  }

  case class OperationType(name: UniqueName, resultType: TypedAst.InTypeExpr)(source: SourceTrace = NoSource())
    extends InTypeExpr(source) {

    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case _: SomeOperationType => true
      case OperationType(name2, resultType2) =>
        name == name2 && resultType == resultType2
      case NestedOperationType(operations) =>
        operations.exists(_.name == name)
      case _ => false
    }

    override def customToString: String = s"operation<$name, $resultType>"
  }

  def typesMatch(ts1: List[InTypeExpr], ts2: List[InTypeExpr]): Boolean = {
    ts1.length == ts2.length && ts1.zip(ts2).forall {
      case (t1, t2) => t1 equalsType t2
    }
  }

  sealed abstract class FunctionKind

  object FunctionKind {

    case class FunctionKindDatatypeConstructor() extends FunctionKind

    case class FunctionKindCrdtQuery() extends FunctionKind

  }

  def functionType(argTypes: List[InTypeExpr], returnType: InTypeExpr, functionKind: FunctionKind)(source: SourceTrace = NoSource()): FunctionType
  = FunctionType(argTypes, returnType, functionKind)(source)

  case class FunctionType(argTypes: List[InTypeExpr], returnType: InTypeExpr, functionKind: FunctionKind)(source: SourceTrace = NoSource())
    extends InTypeExpr(source) {

    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case FunctionType(argTypes2, returnType2, kind) =>
        returnType.equalsType(returnType2) &&
          functionKind == kind &&
          typesMatch(argTypes, argTypes2)
      case _ => false
    }

    override def customToString: String = s"(${argTypes.mkString(", ")}) => $returnType"
  }

  /** a type expression with a name */
  sealed abstract class InTypeExprNamed(source: SourceTrace) extends InTypeExpr(source) {
    def name: UniqueName
  }

  case class SimpleType(name: UniqueName)(source: SourceTrace = NoSource()) extends InTypeExprNamed(source) {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case SimpleType(name2) => name == name2
      case _ => false
    }

    override def customToString: String = name.toString
  }

  case class IdType(name: UniqueName)(source: SourceTrace = NoSource()) extends InTypeExprNamed(source) {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case IdType(name2) => name == name2
      case _ => false
    }

    override def customToString: String = name.toString
  }

  case class CrdtTypeDefinitionType(crdt: CrdtTypeDefinition) extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean =
      other == this

    override def customToString: String =
      s"CRDT#${crdt.name}"
  }

  case class NestedOperationType(operations: List[Operation]) extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean =
      this == other

    override def customToString: String = s"NestedOperations(${operations.map(_.name).mkString(", ")})"
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
