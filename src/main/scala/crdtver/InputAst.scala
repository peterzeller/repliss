package crdtver

import crdtver.BoogieAst.{Forall, ProcCall, _}
import crdtver.parser.LangParser._
import crdtver.parser.{LangBaseVisitor, LangParser}
import org.antlr.v4.runtime.{ParserRuleContext, Token}

import scala.collection.JavaConversions._

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

object InputAst {

  sealed abstract class AstElem(source: ParserRuleContext) {

    def getSource() = source
  }

  case class InProgram(
    source: ProgramContext,
    procedures: List[InProcedure],
    types: List[InTypeDecl],
    operations: List[InOperationDecl],
    queries: List[InQueryDecl],
    axioms: List[InAxiomDecl],
    invariants: List[InInvariantDecl]
  ) extends AstElem(source)

  sealed abstract class InDeclaration(source: ParserRuleContext) extends AstElem(source: ParserRuleContext) {

  }

  case class InProcedure(
    source: ParserRuleContext,
    name: Identifier,
    params: List[InVariable],
    returnType: Option[InTypeExpr],
    body: InStatement
  ) extends InDeclaration(source)

  case class InTypeDecl(
    source: ParserRuleContext,
    isIdType: Boolean,
    name: Identifier,
    dataTypeCases: List[DataTypeCase]
  ) extends InDeclaration(source)

  case class DataTypeCase(
    source: ParserRuleContext,
    name: Identifier,
    params: List[InVariable]
  ) extends AstElem(source)


  case class InOperationDecl(
    source: ParserRuleContext,
    name: Identifier,
    params: List[InVariable]
  ) extends InDeclaration(source)

  case class InQueryDecl(
    source: ParserRuleContext,
    name: Identifier,
    params: List[InVariable],
    returnType: InTypeExpr,
    implementation: InExpr
  ) extends InDeclaration(source)

  case class InAxiomDecl(
    source: ParserRuleContext,
    expr: InExpr
  ) extends InDeclaration(source)

  case class InInvariantDecl(
    source: ParserRuleContext,
    expr: InExpr
  ) extends InDeclaration(source)


  case class Identifier(source: ParserRuleContext, name: String) extends AstElem(source) {
    override def toString() = name
  }

  case class InVariable(
    source: ParserRuleContext,
    name: Identifier,
    typ: InTypeExpr)
    extends AstElem(source)

  sealed abstract class InExpr(source: ParserRuleContext, typ: InTypeExpr)
    extends AstElem(source: ParserRuleContext) {

  }

  case class VarUse(
    source: ParserRuleContext,
    typ: InTypeExpr,
    name: String
  ) extends InExpr(source, typ)


  case class FieldAccess(
    source: ParserRuleContext,
    typ: InTypeExpr,
    receiver: InExpr,
    fieldName: Identifier
  ) extends InExpr(source, typ)


  case class FunctionCall(
    source: ParserRuleContext,
    typ: InTypeExpr,
    functionName: Identifier,
    args: List[InExpr]
  ) extends InExpr(source, typ)


  case class ApplyBuiltin(
    source: ParserRuleContext,
    typ: InTypeExpr,
    function: BuiltInFunc,
    args: List[InExpr]
  ) extends InExpr(source, typ)

  case class QuantifierExpr(
    source: ParserRuleContext,
    typ: InTypeExpr,
    quantifier: Quantifier,
    expr: InExpr
  ) extends InExpr(source, typ)


  sealed abstract class Quantifier

  case class Forall() extends Quantifier

  case class Exists() extends Quantifier

  sealed abstract class BuiltInFunc

  case class BF_isVisible() extends BuiltInFunc()

  case class BF_happensBefore() extends BuiltInFunc()

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


  sealed abstract class InStatement(source: ParserRuleContext)
    extends AstElem(source: ParserRuleContext) {

  }

  case class BlockStmt(
    source: ParserRuleContext,
    stmts: List[InStatement]
  ) extends InStatement(source)

  case class Atomic(
    source: ParserRuleContext,
    body: InStatement
  ) extends InStatement(source)



  case class LocalVar(
    source: ParserRuleContext,
    variable: InVariable
  ) extends InStatement(source)



  case class IfStmt(
    source: ParserRuleContext,
    cond: InExpr,
    thenStmt: InStatement,
    elseStmt: InStatement
  ) extends InStatement(source)


  case class CrdtCall(
    source: ParserRuleContext,
    call: FunctionCall
  ) extends InStatement(source)


  case class Assignment(
    source: ParserRuleContext,
    varname: Identifier,
    expr: InExpr
  ) extends InStatement(source)


  case class NewIdStmt(
    source: ParserRuleContext,
    varname: Identifier,
    typename: InTypeExpr
  ) extends InStatement(source)


  case class ReturnStmt (
    source: ParserRuleContext,
    expr: InExpr
  ) extends InStatement(source)







  sealed abstract class InTypeExpr(source: ParserRuleContext)
    extends AstElem(source: ParserRuleContext) {

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
    isInAtomic: Boolean = false
  )


  def transformProgram(programContext: ProgramContext): InProgram = {
    val procedures = programContext.declaration().asScala.flatMap(d => Option(d.procedure())).toList
    val typeDecls = programContext.declaration().asScala.flatMap(d => Option(d.typedecl())).toList
    val operations = programContext.declaration().asScala.flatMap(d => Option(d.operationDecl()).toList
    val queries = programContext.declaration().asScala.flatMap(d => Option(d.queryDecl())).toList
    val axioms = programContext.declaration().asScala.flatMap(d => Option(d.axiomDecl())).toList
    val invariants = programContext.declaration().asScala.flatMap(d => Option(d.invariant())).toList


    InProgram(
      source = programContext,
      procedures = ???,
      types = ???,
      operations = ???,
      queries = ???,
      axioms = ???,
      invariants = ???
    )
  }

  def sortTypes(types: Iterable[TypeDecl], constructors: List[FuncDecl]): List[Declaration] = {
    var result = List[Declaration]()

    for (t <- types) {
      result = result ++ List(t) ++ (for (constr <- constructors; if constr.resultType == SimpleType(t.name)) yield constr)
    }

    result
  }


  def makeProcBeginAtomic(): Procedure = {

    Procedure(
      name = "beginAtomic",
      inParams = List(),
      outParams = List(),
      requires = List(),
      modifies = List(IdentifierExpr("state_visibleCalls")),
      ensures = List(
        // well formed history:
        Ensures(isFree = true,
          FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name)))),
        // set of visible updates can grow:
        Ensures(isFree = true,
          Forall("c" :: typeCallId, Old("state_visibleCalls".get("c"))
            ==> "state_visibleCalls".get("c"))),
        // causally consistent:
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            "state_visibleCalls".get("c2") && "state_happensBefore".get("c1", "c2")
              ==> "state_visibleCalls".get("c1"))),
        // transaction consistent:
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            "state_visibleCalls".get("c1") && "state_sameTransaction".get("c1", "c2")
              ==> "state_visibleCalls".get("c2")))
      ),
      body = Block()
    )

  }

  def makeProcEndAtomic(): Procedure = {

    // TODO should add operations from current transaction?

    // TODO should check invariant after endAtomic?

    Procedure(
      name = "endAtomic",
      inParams = List(),
      outParams = List(),
      requires = invariants.map(Requires(false, _)),
      modifies = List(),
      ensures = List(),
      body = Block()
    )

  }


  def makeProcCrdtOperation(): Procedure = {

    val state_maxId: Expr = "state_maxId"
    val newCallId: Expr = "CallId" $ (Old(state_maxId) + IntConst(1))

    Procedure(
      name = "crdtOperation",
      inParams = List("operation" :: typeOperation),
      outParams = List(),
      requires = List(
        Requires(isFree = false, FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name))))
      ),
      modifies = List("state_callOps", "state_happensBefore", "state_visibleCalls", "state_sameTransaction", "state_currentTransaction", "state_maxId"),
      ensures = List(
        Ensures(isFree = true,
          Old("state_callOps".get(newCallId)) === ("noop" $())),
        Ensures(isFree = true, "state_callOps".get(newCallId) === "operation"),
        Ensures(isFree = true, Forall("c1" :: typeCallId, ("c1" !== newCallId) ==> ("state_callOps".get("c1") === Old("state_callOps".get("c1"))))),
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            "state_happensBefore".get("c1", "c2")
              <==> (Old("state_happensBefore".get("c1", "c2"))
              || (("state_visibleCalls".get("c1") || "c1" === "c2") && "c2" === newCallId)))),
        Ensures(isFree = true,
          Forall("c1" :: typeCallId, "state_visibleCalls".get("c1")
            <==> (Old("state_visibleCalls".get("c1")) || "c1" === newCallId)))
        // TODO update current transaction and sameTransaction
        , Ensures(isFree = true,
          FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name))))
      ),
      body = Block()
    )

  }

  def makeFunc_WellFormed(): FuncDecl = {
    val i: Expr = "i"
    val state_maxId: Expr = "state_maxId"


    FuncDecl(
      name = "WellFormed",
      arguments = stateVars.map(g => VarDecl(g.name, g.typ)),
      resultType = TypeBool(),
      implementation = Some(
        // no happensBefore relation between non-existing calls
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (("state_callOps".get("c1") === ("noop" $())) || ("state_callOps".get("c2") === ("noop" $())))
            ==> !"state_happensBefore".get("c1", "c2")
        )
          // visible calls are a subset of all calls
          && Forall("c" :: typeCallId, "state_visibleCalls".get("c") ==> ("state_callOps".get("c") !== ("noop" $())))
          // happensBefore is a partial order (reflexivity, transitivity, antisymmetric)
          && Forall("c" :: typeCallId, ("state_callOps".get("c") !== ("noop" $())) ==> "state_happensBefore".get("c", "c"))
          && Forall(List("x" :: typeCallId, "y" :: typeCallId, "z" :: typeCallId),
          ("state_happensBefore".get("x", "y") && "state_happensBefore".get("y", "z")) ==> "state_happensBefore".get("x", "z"))
          && Forall(List("x" :: typeCallId, "y" :: typeCallId), ("state_happensBefore".get("x", "y") && "state_happensBefore".get("y", "x")) ==> ("x" === "y"))
          && Forall("i" :: SimpleType("int"), (i >= state_maxId) ==> ("state_callOps".get("CallId" $ (i)) === ("noop" $())))
        // TODO infinitely many free ids
      )
    )
  }


  def transformLocals(body: StmtContext): Statement = {
    var locals = List[Statement]()
    val listener = new LangBaseVisitor[Unit] {
      override def visitLocalVar(lv: LangParser.LocalVarContext): Unit = {
        locals +:= transformLocalVar(lv)
      }
    }
    body.accept(listener)
    makeBlock(locals)
  }

  def transformProcedure(procedure: ProcedureContext): Procedure = {


    val procname: String = procedure.name.getText
    Procedure(
      name = procname,
      inParams = procedure.params.toList.map(transformVariable),
      outParams =
        if (procedure.returnType == null) List()
        else List(VarDecl("result", transformTypeExpr(procedure.returnType))),
      requires =
        Requires(isFree = false, FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name))))
          +: invariants.map(Requires(false, _))
      ,
      modifies = stateVars.map(g => IdentifierExpr(g.name)),
      ensures = invariants.map(Ensures(false, _)),
      body = makeBlock(
        transformLocals(procedure.body),
        captureState(procedure.start, s"start of procedure $procname"),
        transformStatement(procedure.body)(Context()),
        captureState(procedure.stop, s"end of procedure $procname"))
    )
  }

  def transformVariable(variable: VariableContext): VarDecl =
    VarDecl(variable.name.getText, transformTypeExpr(variable.`type`()))


  def transformBlockStmt(context: BlockStmtContext)(implicit ctxt: Context): Statement = {
    makeBlock(context.stmt().toList.map(transformStatement))
  }

  def transformAtomicStmt(context: AtomicStmtContext)(implicit ctxt: Context): Statement = makeBlock(
    ProcCall(None, "beginAtomic", List()),
    captureState(context.start, "begin atomic"),
    transformStatement(context.stmt())(ctxt.copy(isInAtomic = true)),
    captureState(context.stop, "before commit"),
    ProcCall(None, "endAtomic", List()),
    captureState(context.stop, "end atomic")
  )

  def transformLocalVar(context: LocalVarContext): Statement = {
    val v = transformVariable(context.variable())
    LocalVar(v.name, v.typ)
  }


  def transformIfStmt(context: IfStmtContext)(implicit ctxt: Context): Statement = {
    IfStmt(transformExpr(context.condition),
      transformStatement(context.thenStmt),
      transformStatement(context.elseStmt))
  }


  def transofrmCrdtCall(context: CrdtCallContext)(implicit ctxt: Context): Statement = {
    val call = ProcCall(None, "crdtOperation", List(transformFunctioncall(context.functionCall())))
    if (ctxt.isInAtomic) {
      call
    } else {
      // database call outside transaction is wrapped in singleton transaction
      Block(
        ProcCall(None, "beginAtomic", List()),
        captureState(context.start, "begin atomic"),
        call,
        captureState(context.start, "before commit"),
        ProcCall(None, "endAtomic", List()),
        captureState(context.stop, "end atomic")
      )
    }
  }

  def transformAssignment(context: AssignmentContext): Statement = {
    Assignment(context.varname.getText, transformExpr(context.expr()))
  }

  def transformStatement(stmt: StmtContext)(implicit ctxt: Context): Statement = {
    if (stmt == null)
      return Block()
    makeBlock(
      captureState(stmt.start),
      transformStatement2(stmt))
  }

  def captureState(source: Token, msg: String = ""): Assume = {
    Assume(BoolConst(true), List(Attribute("captureState", List(Left("[line " + source.getLine + ":" + source.getCharPositionInLine + "] " + msg)))))
  }


  def transformStatement2(stmt: StmtContext)(implicit ctxt: Context): Statement = {
    if (stmt.blockStmt() != null) {
      transformBlockStmt(stmt.blockStmt())
    } else if (stmt.atomicStmt() != null) {
      transformAtomicStmt(stmt.atomicStmt())
    } else if (stmt.localVar() != null) {
      // transformLocalVar(stmt.localVar())
      // was already translated at beginning of procedure
      Block()
    } else if (stmt.ifStmt() != null) {
      transformIfStmt(stmt.ifStmt())
    } else if (stmt.crdtCall() != null) {
      transofrmCrdtCall(stmt.crdtCall())
    } else if (stmt.assignment() != null) {
      transformAssignment(stmt.assignment())
    } else if (stmt.newIdStmt() != null) {
      transformNewIdStmt(stmt.newIdStmt())
    } else if (stmt.returnStmt() != null) {
      transformReturnStmt(stmt.returnStmt())
    } else {
      throw new RuntimeException("unhandled case: " + stmt.toStringTree(parser))
    }
  }

  def transformExpr(e: ExprContext): Expr = {
    if (e.varname != null) {
      IdentifierExpr(e.varname.getText)
    } else if (e.operator != null) {
      e.operator.getText match {
        case "before" =>
          Lookup("state_happensBefore", List(transformExpr(e.left), transformExpr(e.right)))
        case "after" =>
          Lookup("state_happensBefore", List(transformExpr(e.right), transformExpr(e.left)))
        case op =>
          FunctionCall(op, List(transformExpr(e.left), transformExpr(e.right)))
      }
    } else if (e.quantifierExpr() != null) {
      transformQuantifierExpr(e.quantifierExpr())
    } else if (e.functionCall() != null) {
      transformFunctioncall(e.functionCall())
    } else if (e.parenExpr != null) {
      transformExpr(e.parenExpr)
    } else if (e.isAttribute != null) {
      Lookup("state_visibleCalls", List(transformExpr(e.left)))
    } else if (e.receiver != null) {
      val receiver = transformExpr(e.receiver)
      e.fieldName.getText match {
        case "op" => Lookup("state_callOps", List(receiver))
        case "info" => Lookup("state_invocations", List(receiver))
        case "origin" => Lookup("state_origin", List(receiver))
      }
    } else if (e.unaryOperator != null) {
      FunctionCall(e.unaryOperator.getText, List(transformExpr(e.right)))
    } else {
      throw new RuntimeException("unhandled case: " + e.toStringTree(parser))
    }
  }

  def transformNewIdStmt(context: NewIdStmtContext): Statement = {
    val varName: String = context.varname.getText
    val typeName: String = context.typename.getText
    Block(
      // nondeterministic creation of new id
      Havoc(varName)
        // we can assume that the new id was never used in an operation before
        :: newIdAssumptions(typeName, varName)
    )

  }

  def newIdAssumptions(typeName: String, idName: String): List[Statement] = {
    // add axioms for contained ids
    var result = List[Statement]()
    for ((opName, args) <- operationDefs) {
      val idType = SimpleType(typeName)
      val argIds: List[IdentifierExpr] = args.map(a => IdentifierExpr(a.name))
      result = result ++ (for (arg <- args; if arg.typ == idType) yield {
        Assume(Forall(("c" :: typeCallId) +: args, ("state_callOps".get("c") === FunctionCall(opName, argIds)) ==> (IdentifierExpr(idName) !== arg.name)))
      })
    }
    result
  }

  def transformReturnStmt(context: ReturnStmtContext): Statement = {
    Return(transformExpr(context.expr()))
  }


  def transformFunctioncall(context: FunctionCallContext): FunctionCall = {
    var funcName: String = context.funcname.getText
    var args: List[Expr] = context.args.toList.map(transformExpr)
    if (queryFunctions.contains(funcName)) {
      // add state vars for query-functions
      args ++= stateVars.map(g => IdentifierExpr(g.name))
    }
    if (procedureNames.contains(funcName)) {
      // add invocation name
      funcName = "invocation_" + funcName;
    }
    FunctionCall(funcName, args)
  }

  def transformQuantifierExpr(q: QuantifierExprContext): Expr = {
    val vars = q.vars.toList.map(transformVariable)
    val e = transformExpr(q.expr())
    q.quantifier.getText match {
      case "forall" => Forall(vars, e)
      case "exists" => Exists(vars, e)
    }

  }


  def transformTypeExpr(t: TypeContext): TypeExpr = {
    val typeName: String = t.name.getText
    if (typeName == "Boolean") {
      TypeBool()
    } else {
      SimpleType(typeName)
    }

  }

}
