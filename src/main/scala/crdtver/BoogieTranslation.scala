package crdtver

import crdtver.parser.LangParser._
import crdtver.BoogieAst._
import crdtver.parser.LangParser

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

class BoogieTranslation(val parser: LangParser) {

  var types: Map[String, TypeDecl] = Map()
  var stateVars: List[GlobalVariable] = List()

  var invariants: List[Expr] = List()

  val typeCallId = SimpleType("callId")
  val typeOperation = SimpleType("operation")

  def transformProgram(programContext: ProgramContext): Program = {


    stateVars = List(
      GlobalVariable("state_callOps", MapType(List(typeCallId), typeOperation)),
      GlobalVariable("state_visibleCalls", MapType(List(typeCallId), TypeBool())),
      GlobalVariable("state_happensBefore", MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable("state_sameTransaction", MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable("state_currentTransaction", MapType(List(typeCallId), TypeBool()))
    )

    types += ("callId" -> TypeDecl("callId"))

    for (decl: DeclarationContext <- programContext.declaration();
         typeDecl: TypedeclContext <- Option(decl.typedecl())) {
      val name: String = typeDecl.name.getText
      types += (typeDecl.name.getText -> TypeDecl(name))
    }

    val standardProcedures = List(
      makeProcBeginAtomic(),
      makeProcEndAtomic(),
      makeProcCrdtOperation()
    )

    val translatedProcedures = for (decl: DeclarationContext <- programContext.declaration();
         procedure: ProcedureContext <- Option(decl.procedure())) yield {
      transformProcedure(procedure)
    }


    Program(List() ++ types.values ++ stateVars ++ standardProcedures ++ translatedProcedures)
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
          "WellFormed"$("state_callOps", "state_visibleCalls", "state_happensBefore")),
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
      requires = List(),
      modifies = List(),
      ensures = List(),
      body = Block()
    )

  }


  def makeProcCrdtOperation(): Procedure = {


    Procedure(
      name = "crdtOperation",
      inParams = List("operation" :: typeOperation),
      outParams = List(),
      requires = List(),
      modifies = List("state_callOps", "state_happensBefore", "state_visibleCalls", "state_sameTransaction", "state_currentTransaction"),
      ensures = List(
        Ensures(isFree = true,
          Exists("c" :: typeCallId,
            Old("state_callOps".get("c")) === ("noop"$())
              && "state_callOps".get("c") === "operation"
              && Forall("c1" :: typeCallId, "state_callOps".get("c1") === Old("state_callOps".get("c1")))
              && Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
              "state_happensBefore".get("c1", "c2")
                <==> (Old("state_happensBefore".get("c1", "c2"))
                || Old("state_visibleCalls".get("c1")) && "c2" === "c"))
              && Forall("c1" :: typeCallId, "state_visibleCalls".get("c1")
              <==> (Old("state_visibleCalls".get("c1")) || "c1" === "c"))
            // TODO update current transaction and sameTransaction
          )
        )
      ),
      body = Block()
    )

  }


  def transformProcedure(procedure: ProcedureContext): Procedure = {


    Procedure(
      name = procedure.name.getText,
      inParams = procedure.params.toList.map(transformVariable),
      outParams =
        if (procedure.returnType == null) List()
        else List(VarDecl("result", transformTypeExpr(procedure.returnType))),
      requires =
        Requires(isFree = false, "WellFormed"$("state_callOps", "state_visibleCalls", "state_happensBefore"))
          +: invariants.map(Requires(false, _))
      ,
      modifies = stateVars.map(g => IdentifierExpr(g.name)),
      ensures = invariants.map(Ensures(false, _)),
      body = transformStatement(procedure.body)
    )
  }

  def transformVariable(variable: VariableContext): VarDecl =
    VarDecl(variable.name.getText, transformTypeExpr(variable.`type`()))




  def transformBlockStmt(context: BlockStmtContext): Statement = {
    Block(context.stmt().toList.map(transformStatement))
  }

  def transformAtomicStmt(context: AtomicStmtContext): Statement = makeBlock(
    ProcCall(None, "beginAtomic", List()),
    transformStatement(context.stmt()),
    ProcCall(None, "endAtomic", List())
  )

  def transformLocalVar(context: LocalVarContext): Statement = {
    val v = transformVariable(context.variable())
    LocalVar(v.name, v.typ)
  }



  def transformIfStmt(context: IfStmtContext): Statement = {
    IfStmt(transformExpr(context.condition),
      transformStatement(context.thenStmt),
      transformStatement(context.elseStmt))
  }



  def transofrmCrdtCall(context: CrdtCallContext): Statement = {
    ProcCall(None, "crdtOperation", List(transformFunctioncall(context.functionCall())))
  }

  def transformAssignment(context: AssignmentContext): Statement = {
    Assignment(context.varname.getText, transformExpr(context.expr()))
  }

  def transformStatement(stmt: StmtContext): Statement = {
    if (stmt == null) {
      Block()
    } else if (stmt.blockStmt() != null) {
      transformBlockStmt(stmt.blockStmt())
    } else if (stmt.atomicStmt() != null) {
      transformAtomicStmt(stmt.atomicStmt())
    } else if (stmt.localVar() != null) {
      transformLocalVar(stmt.localVar())
    } else if (stmt.ifStmt() != null) {
      transformIfStmt(stmt.ifStmt())
    } else if (stmt.crdtCall() != null) {
      transofrmCrdtCall(stmt.crdtCall())
    } else if (stmt.assignment() != null) {
      transformAssignment(stmt.assignment())
    } else {
      throw new RuntimeException("unhandled case: " + stmt.toStringTree(parser))
    }
  }



  def transformExpr(e: ExprContext): Expr = {
    if (e.varname != null) {
      IdentifierExpr(e.varname.getText)
    } else if (e.operator != null) {
      FunctionCall(e.operator.getText, List(transformExpr(e.left), transformExpr(e.right)))
    } else if (e.quantifierExpr() != null) {
      transformQuantifierExpr(e.quantifierExpr())
    } else if (e.functionCall() != null) {
      transformFunctioncall(e.functionCall())
    } else if (e.parenExpr != null) {
      transformExpr(e.parenExpr)
    } else {
      throw new RuntimeException("unhandled case: " + e.toStringTree(parser))
    }
  }


  def transformFunctioncall(context: FunctionCallContext): FunctionCall =
    FunctionCall(context.funcname.getText, context.args.toList.map(transformExpr))

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
    if (typeName == "boolean") {
      TypeBool()
    } else {
      SimpleType(typeName)
    }

  }

}
