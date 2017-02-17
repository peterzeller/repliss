package crdtver

import crdtver.parser.LangParser._
import crdtver.parser.{LangBaseVisitor, LangParser}
import org.antlr.v4.runtime.{ParserRuleContext, Token}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

object InputAst {

  sealed abstract class AstElem(source: SourceTrace) {

    def getSource() = source

    override def toString: String = customToString

    def customToString: String
  }

  case class InProgram(
    source: ProgramContext,
    procedures: List[InProcedure],
    types: List[InTypeDecl],
    operations: List[InOperationDecl],
    queries: List[InQueryDecl],
    axioms: List[InAxiomDecl],
    invariants: List[InInvariantDecl]
  ) extends AstElem(source) {
    override def customToString: String = "program"
  }

  sealed abstract class InDeclaration(source: SourceTrace) extends AstElem(source: SourceTrace) {

  }

  case class InProcedure(
    source: SourceTrace,
    name: Identifier,
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
    name: Identifier,
    dataTypeCases: List[DataTypeCase]
  ) extends InDeclaration(source) {
    override def customToString: String = s"type $name"

  }

  case class DataTypeCase(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable]
  ) extends AstElem(source) {
    override def customToString: String = s"datatype case $name"
  }


  case class InOperationDecl(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable]
  ) extends InDeclaration(source) {
    override def customToString: String = s"operation $name"
  }

  case class InQueryDecl(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable],
    returnType: InTypeExpr,
    implementation: Option[InExpr],
    annotations: Set[InAnnotation]
  ) extends InDeclaration(source) {
    override def customToString: String = s"query $name"
  }

  sealed trait InAnnotation
  case class InlineAnnotation() extends InAnnotation

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

  case class SourceRange(start: SourcePosition, stop: SourcePosition) {
    override def toString: String = s"$start-$stop"
  }

  case class SourcePosition(line: Int, column: Int) {
    override def toString: String = s"$line:$column"
  }

  implicit def tokenToSourcePosition(t: Token): SourcePosition = {
    SourcePosition(t.getLine, t.getCharPositionInLine)
  }

  sealed abstract class SourceTrace {
    def stop: SourcePosition

    def start: SourcePosition

    def range: SourceRange = SourceRange(start, stop)

    def getLine: Int = start.line
  }

  case class ParserRuleSource(source: ParserRuleContext) extends SourceTrace {

    override def stop: SourcePosition = SourcePosition(source.stop.getLine, source.stop.getCharPositionInLine + source.stop.getText.length)

    override def start: SourcePosition = source.start
  }

  implicit def parserRuleContextToSourceTrace(source: ParserRuleContext): SourceTrace = ParserRuleSource(source)

  case class TokenSource(source: Token) extends SourceTrace {

    override def stop: SourcePosition = SourcePosition(source.getLine, source.getCharPositionInLine + source.getText.length)

    override def start: SourcePosition = source
  }

  implicit def parserRuleContextToSourceTrace(source: Token): SourceTrace = TokenSource(source)

  case class NoSource() extends SourceTrace {
    override def getLine: Int = 0

    override def stop: SourcePosition = SourcePosition(0,0)

    override def start: SourcePosition = SourcePosition(0,0)
  }
  
  case class Identifier(source: SourceTrace, name: String) extends AstElem(source) {
    override def customToString: String = name
  }

  case class InVariable(
    source: SourceTrace,
    name: Identifier,
    typ: InTypeExpr)
    extends AstElem(source) {
    override def customToString: String = s"var $name: $typ"
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
    override def customToString: String = name
  }

  case class BoolConst(
      source: SourceTrace,
      typ: InTypeExpr,
      value: Boolean
    ) extends InExpr(source, typ) {
      override def customToString: String = value.toString
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
    args: List[InExpr]
  ) extends CallExpr(source, typ, args) {
    override def customToString: String = s"$functionName(${args.mkString(", ")})"
  }


  case class ApplyBuiltin(
    source: SourceTrace,
    typ: InTypeExpr,
    function: BuiltInFunc,
    args: List[InExpr]
  ) extends CallExpr(source, typ, args) {
    override def customToString: String = {
      function match {
        case BF_isVisible() => s"${args.head} is visible"
        case BF_happensBefore() => s"(${args.head} happens before ${args(1)})"
        case BF_sameTransaction() => s"sameTransaction(${args(0)}, ${args(1)})"
        case BF_less() => s"(${args.head} < ${args(1)})"
        case BF_lessEq() =>s"(${args.head} <= ${args(1)})"
        case BF_greater() =>s"(${args.head} > ${args(1)})"
        case BF_greaterEq() =>s"(${args.head} >= ${args(1)})"
        case BF_equals() =>s"(${args.head} == ${args(1)})"
        case BF_notEquals() =>s"(${args.head} != ${args(1)})"
        case BF_and() =>s"(${args.head} && ${args(1)})"
        case BF_or() =>s"(${args.head} || ${args(1)})"
        case BF_implies() =>s"(${args.head} ==> ${args(1)})"
        case BF_not() => s"!(${args.head})"
        case BF_getOperation() =>s"${args.head}.op"
        case BF_getInfo() =>s"${args.head}.info"
        case BF_getResult() =>s"${args.head}.result"
        case BF_getOrigin() =>s"${args.head}.origin"
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
  }


  sealed abstract class Quantifier

  case class Forall() extends Quantifier {
    override def toString: String = "forall"
  }

  case class Exists() extends Quantifier {
    override def toString: String = "exists"
  }

  sealed abstract class BuiltInFunc

  case class BF_isVisible() extends BuiltInFunc()

  case class BF_happensBefore() extends BuiltInFunc()

  case class BF_sameTransaction() extends BuiltInFunc()

  case class BF_less() extends BuiltInFunc()

  case class BF_lessEq() extends BuiltInFunc()

  case class BF_greater() extends BuiltInFunc()

  case class BF_greaterEq() extends BuiltInFunc()

  case class BF_equals() extends BuiltInFunc()

  case class BF_notEquals() extends BuiltInFunc()

  case class BF_and() extends BuiltInFunc()

  case class BF_or() extends BuiltInFunc()

  case class BF_implies() extends BuiltInFunc()

  case class BF_not() extends BuiltInFunc()

  case class BF_getOperation() extends BuiltInFunc()

  case class BF_getInfo() extends BuiltInFunc()

  case class BF_getResult() extends BuiltInFunc()

  case class BF_getOrigin() extends BuiltInFunc()

  case class BF_inCurrentInvoc() extends BuiltInFunc()


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

  private def flatten(s: InStatement): List[InStatement] =  s match {
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
    pattern: InExpr,
    statement: InStatement
  ) extends AstElem(source) {

    override def customToString: String = s"case $pattern => $statement"
  }



  case class CrdtCall(
    source: SourceTrace,
    call: FunctionCall
  ) extends InStatement(source) {
    override def customToString: String = s"call $call"
  }


  case class Assignment(
    source: SourceTrace,
    varname: Identifier,
    expr: InExpr
  ) extends InStatement(source) {
    override def customToString: String = s"$varname := $expr"
  }


  case class NewIdStmt(
    source: SourceTrace,
    varname: Identifier,
    typename: InTypeExpr
  ) extends InStatement(source) {
    override def customToString: String = s"$varname := new $typename"
  }


  case class ReturnStmt (
    source: SourceTrace,
    expr: InExpr,
    assertions: List[AssertStmt]
  ) extends InStatement(source) {
    override def customToString: String = s"return $expr"
  }


  case class AssertStmt (
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

  case class UnknownType() extends InTypeExpr {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = false

    override def customToString: String = "unknown"
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

  case class OperationType(name: String, source: SourceTrace = NoSource())
    extends InTypeExpr(source) {

    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case _: SomeOperationType => true
      case OperationType(name2,  _) =>
        name == name2
      case _ => false
    }

    override def customToString: String = s"operation<$name>"
  }

  def typesMatch(ts1: List[InTypeExpr], ts2: List[InTypeExpr]): Boolean = {
    ts1.length == ts2.length && ts1.zip(ts2).forall {
      case (t1,t2) => t1 equalsType t2
    }
  }

  case class FunctionType(argTypes: List[InTypeExpr], returnType: InTypeExpr, source: SourceTrace = NoSource())
    extends InTypeExpr(source) {

    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case FunctionType(argTypes2, returnType2, _) =>
        returnType.equalsType(returnType2) && typesMatch(argTypes, argTypes2)
      case _ => false
    }

    override def customToString: String = s"(${argTypes.mkString(", ")}) => $returnType"
  }

  case class SimpleType(name: String, source: SourceTrace = NoSource()) extends InTypeExpr(source) {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case SimpleType(name2, _) => name == name2
      case _ => false
    }

    override def customToString: String = name
  }

  case class IdType(name: String, source: SourceTrace = NoSource()) extends InTypeExpr(source) {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = other match {
      case IdType(name2, _) => name == name2
      case _ => false
    }

    override def customToString: String = name
  }


  case class UnresolvedType(name: String, source: SourceTrace = NoSource()) extends InTypeExpr(source) {
    override def isSubtypeOfIntern(other: InTypeExpr): Boolean = false

    override def customToString: String = s"unresoved<$name>"
  }


//  var types: Map[String, TypeDecl] = Map()
//  var datatypeConstructors: List[FuncDecl] = List()
//  var stateVars: List[GlobalVariable] = List()
//
//  var queryFunctions: Map[String, FuncDecl] = Map()
//
//  var invariants: List[Expr] = List()
//
//  val typeCallId = SimpleType("callId")
//  val typeInvocationId = SimpleType("invocationId")
//  val typeInvocationInfo = SimpleType("invocationInfo")
//  val typeOperation = SimpleType("operation")
//
//  var newIdTypes: List[String] = List()
//
//  var operationDefs: List[(String, List[VarDecl])] = List()
//
//  var procedures = List[ProcedureContext]()
//  var procedureNames = Set[String]()

  case class Context(
    programContext: ProgramContext,
    isInAtomic: Boolean = false
  )


  def transformProgram(programContext: ProgramContext): InProgram = {
    val procedures = programContext.declaration().asScala.flatMap(d => Option(d.procedure())).toList
    val typeDecls = programContext.declaration().asScala.flatMap(d => Option(d.typedecl())).toList
    val operations = programContext.declaration().asScala.flatMap(d => Option(d.operationDecl())).toList
    val queries = programContext.declaration().asScala.flatMap(d => Option(d.queryDecl())).toList
    val axioms = programContext.declaration().asScala.flatMap(d => Option(d.axiomDecl())).toList
    val invariants = programContext.declaration().asScala.flatMap(d => Option(d.invariant())).toList

    implicit val ctxt = Context(programContext)

    InProgram(
      source = programContext,
      procedures = procedures.map(transformProcedure),
      types = typeDecls.map(transformTypeDecl),
      operations = operations.map(transformOperation),
      queries = queries.map(transformQuery),
      axioms = axioms.map(transformAxiom),
      invariants = invariants.map(transformInvariant)
    )
  }

  def transformInvariant(a: InvariantContext): InInvariantDecl = {
    InInvariantDecl(a, transformExpr(a.expr()))
  }

  def transformAxiom(a: AxiomDeclContext): InAxiomDecl = {
    InAxiomDecl(a, transformExpr(a.expr()))
  }

  def transformOperation(o: OperationDeclContext): InOperationDecl = {
    InOperationDecl(
      source = o,
      name = makeIdentifier(o.name),
      params = o.params.map(transformVariable).toList
    )
  }

  def transformQuery(o: QueryDeclContext): InQueryDecl = {
    var annotations = Set[InAnnotation]()
    if (o.inline != null) {
      annotations += InlineAnnotation()
    }

    InQueryDecl(
      source = o,
      name = makeIdentifier(o.name),
      params = o.params.map(transformVariable).toList,
      returnType = transformTypeExpr(o.returnType),
      implementation = Option(o.expr()).map(transformExpr),
      annotations = annotations
    )
  }

  def transformTypeDecl(t: TypedeclContext): InTypeDecl = {
    InTypeDecl(
      source = t,
      isIdType = t.kind.getText == "idtype",
      name = makeIdentifier(t.name),
      dataTypeCases = t.dataTypeCases.map(transformDataTypeCase).toList
    )
  }

  def transformDataTypeCase(c: DataTypeCaseContext): DataTypeCase = {
    DataTypeCase(
      source = c,
      name = makeIdentifier(c.name),
      params = c.params.map(transformVariable).toList
    )
  }


  def makeIdentifier(name: Token): Identifier = {
    Identifier(name, name.getText)
  }

  def transformProcedure(procedure: ProcedureContext): InProcedure = {


    InProcedure(
      source = procedure,
      name = makeIdentifier(procedure.name),
      params = procedure.params.toList.map(transformVariable),
      locals = transformLocals(procedure.body),
      returnType = Option(procedure.returnType).map(transformTypeExpr),
      body = transformStatement(procedure.body)

    )
  }

  def transformLocals(body: StmtContext): List[InVariable] = {
    var locals = List[LocalVar]()
    val listener = new LangBaseVisitor[Unit] {
      override def visitLocalVar(lv: LangParser.LocalVarContext): Unit = {
        locals +:= transformLocalVar(lv)
      }
    }
    body.accept(listener)
    locals.map(_.variable)
  }

  def transformVariable(variable: VariableContext): InVariable =
    InVariable(variable, makeIdentifier(variable.name), transformTypeExpr(variable.`type`()))


  def transformBlockStmt(context: BlockStmtContext): InStatement = {
    BlockStmt(context, context.stmt().toList.map(transformStatement))
  }

  def transformAtomicStmt(context: AtomicStmtContext): InStatement =
    Atomic(context, transformStatement(context.stmt()))

  def transformLocalVar(context: LocalVarContext): LocalVar = {
    val v = transformVariable(context.variable())
    LocalVar(context, v)
  }


  def transformIfStmt(context: IfStmtContext): InStatement = {
    IfStmt(context,
      transformExpr(context.condition),
      transformStatement(context.thenStmt),
      transformStatement(context.elseStmt))
  }


  def transofrmCrdtCall(context: CrdtCallContext): InStatement = {
    transformFunctioncall(context.functionCall()) match {
      case call: FunctionCall =>
        CrdtCall(context, call)
      case _ =>
        // TODO error
        ???
    }
  }

  def transformAssignment(context: AssignmentContext): InStatement = {
    Assignment(context, makeIdentifier(context.varname), transformExpr(context.expr()))
  }

  def transformStatement(stmt: StmtContext): InStatement = {
    if (stmt == null)
      BlockStmt(NoSource(), List())
    else
      transformStatement2(stmt)
  }


  def transformMatchCase(context: MatchCaseContext): MatchCase = {
    MatchCase(
      source = context,
      pattern = transformExpr(context.expr()),
      statement = BlockStmt(context, context.stmt().toList.map(transformStatement))
    )
  }

  def transformMatchStmt(context: MatchStmtContext): InStatement = {

    MatchStmt(
      source = context,
      expr = transformExpr(context.expr()),
      cases = context.cases.toList.map(transformMatchCase)
    )
  }

  def transformStatement2(stmt: StmtContext): InStatement = {
    if (stmt.blockStmt() != null) {
      transformBlockStmt(stmt.blockStmt())
    } else if (stmt.atomicStmt() != null) {
      transformAtomicStmt(stmt.atomicStmt())
    } else if (stmt.localVar() != null) {
      // transformLocalVar(stmt.localVar())
      // was already translated at beginning of procedure
      BlockStmt(stmt, List())
    } else if (stmt.ifStmt() != null) {
      transformIfStmt(stmt.ifStmt())
    } else if (stmt.matchStmt() != null) {
      transformMatchStmt(stmt.matchStmt())
    } else if (stmt.crdtCall() != null) {
      transofrmCrdtCall(stmt.crdtCall())
    } else if (stmt.assignment() != null) {
      transformAssignment(stmt.assignment())
    } else if (stmt.newIdStmt() != null) {
      transformNewIdStmt(stmt.newIdStmt())
    } else if (stmt.returnStmt() != null) {
      transformReturnStmt(stmt.returnStmt())
    } else {
      throw new RuntimeException("unhandled case: " + stmt.toStringTree())
    }
  }

  def transformExpr(e: ExprContext): InExpr = {
    if (e.varname != null) {
      VarUse(e, UnknownType(), e.varname.getText)
    } else if (e.boolval != null) {
      val boolval = e.boolval.getText match {
        case "true" => true
        case "false" => false
      }
      BoolConst(e, BoolType(), boolval)
    } else if (e.operator != null) {
      e.operator.getText match {
        case "before" =>
          ApplyBuiltin(e, UnknownType(), BF_happensBefore(), List(transformExpr(e.left), transformExpr(e.right)))
        case "after" =>
          ApplyBuiltin(e, UnknownType(), BF_happensBefore(), List(transformExpr(e.right), transformExpr(e.left)))
        case op =>
          val bf = op match {
            case "<" => BF_less()
            case "<=" => BF_lessEq()
            case ">" => BF_greater()
            case ">=" => BF_greaterEq()
            case "==" => BF_equals()
            case "!=" => BF_notEquals()
            case "&&" => BF_and()
            case "||" => BF_or()
            case "==>" => BF_implies()
          }
          ApplyBuiltin(e, UnknownType(), bf, List(transformExpr(e.left), transformExpr(e.right)))
      }
    } else if (e.quantifierExpr() != null) {
      transformQuantifierExpr(e.quantifierExpr())
    } else if (e.functionCall() != null) {
      transformFunctioncall(e.functionCall())
    } else if (e.parenExpr != null) {
      transformExpr(e.parenExpr)
    } else if (e.isAttribute != null) {
      ApplyBuiltin(e, UnknownType(), BF_isVisible(), List(transformExpr(e.left)))
    } else if (e.receiver != null) {
      val receiver = transformExpr(e.receiver)
      e.fieldName.getText match {
        case "op" => ApplyBuiltin(e, UnknownType(), BF_getOperation(), List(receiver))
        case "info" => ApplyBuiltin(e, UnknownType(), BF_getInfo(), List(receiver))
        case "result" => ApplyBuiltin(e, UnknownType(), BF_getResult(), List(receiver))
        case "origin" => ApplyBuiltin(e, UnknownType(), BF_getOrigin(), List(receiver))
        case "inCurrentInvocation" => ApplyBuiltin(e, UnknownType(), BF_inCurrentInvoc(), List(receiver))
        case other => FunctionCall(e, UnknownType(), Identifier(e.fieldName, other), List(receiver))
      }
    } else if (e.unaryOperator != null) {
      ApplyBuiltin(e, UnknownType(), BF_not(), List(transformExpr(e.right)))
    } else {
      throw new RuntimeException("unhandled case: " + e.getText)
    }
  }

  def transformNewIdStmt(context: NewIdStmtContext): InStatement = {
    NewIdStmt(context, makeIdentifier(context.varname), UnresolvedType(context.typename.getText))
  }


  def transformReturnStmt(context: ReturnStmtContext): InStatement = {
    ReturnStmt(context, transformExpr(context.expr()), context.assertStmt().toList.map(transformAssertStmt))
  }

  def transformAssertStmt(context: AssertStmtContext): AssertStmt = {
    AssertStmt(context, transformExpr(context.expr()))
  }

  def transformFunctioncall(context: FunctionCallContext): CallExpr = {
    val args: List[InExpr] = context.args.toList.map(transformExpr)
    context.funcname.getText match {
      case "sameTransaction" =>
        ApplyBuiltin(context, UnknownType(), BF_sameTransaction(), args)
      case _ =>
        FunctionCall(context, UnknownType(), makeIdentifier(context.funcname), args)
    }
  }

  def transformQuantifierExpr(q: QuantifierExprContext): InExpr = {
    val vars = q.vars.toList.map(transformVariable)

    val quantifier = q.quantifier.getText match {
      case "forall" => Forall()
      case "exists" => Exists()
    }

    QuantifierExpr(q, UnknownType(), quantifier, vars, transformExpr(q.expr()))
  }


  def transformTypeExpr(t: TypeContext): InTypeExpr = {
    UnresolvedType(t.name.getText, t)
  }

}
