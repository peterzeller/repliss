package crdtver

import crdtver.parser.LangParser._
import crdtver.BoogieAst.{ProcCall, _}
import crdtver.parser.{LangBaseListener, LangBaseVisitor, LangParser}
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.{AbstractParseTreeVisitor, ParseTree}

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

class BoogieTranslation(val parser: LangParser) {

  var types: Map[String, TypeDecl] = Map()
  var datatypeConstructors: List[FuncDecl] = List()
  var stateVars: List[GlobalVariable] = List()

  var queryFunctions: Map[String, FuncDecl] = Map()

  var invariants: List[Expr] = List()

  val typeCallId = SimpleType("callId")
  val typeOperation = SimpleType("operation")


  case class Context(
    isInAtomic: Boolean = false
  )

  def transformProgram(programContext: ProgramContext): Program = {


    stateVars = List(
      GlobalVariable("state_callOps", MapType(List(typeCallId), typeOperation)),
      GlobalVariable("state_visibleCalls", MapType(List(typeCallId), TypeBool())),
      GlobalVariable("state_happensBefore", MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable("state_sameTransaction", MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable("state_currentTransaction", MapType(List(typeCallId), TypeBool()))
    )

    // generate types
    types += ("callId" -> TypeDecl("callId"))

    for (decl: DeclarationContext <- programContext.declaration();
         typeDecl: TypedeclContext <- Option(decl.typedecl())) {
      val name: String = typeDecl.name.getText
      val attributes = if (typeDecl.dataTypeCases.isEmpty) List() else List(Attribute("datatype"))
      types += (name -> TypeDecl(name, attributes))
      for (dtCase <- typeDecl.dataTypeCases) {
        datatypeConstructors +:= FuncDecl(
          name = dtCase.name.getText,
          arguments = dtCase.params.toList.map(transformVariable),
          resultType = SimpleType(name),
          attributes = List(Attribute("constructor"))
        )
      }
    }


    // generate operations
    types += ("operation" -> TypeDecl("operation", attributes = List(Attribute("datatype"))))


    // add noop operation
    datatypeConstructors +:= FuncDecl(
      name = "noop",
      arguments = List(),
      resultType = SimpleType("operation"),
      attributes = List(Attribute("constructor"))
    )

    // add custom operations
    for (decl: DeclarationContext <- programContext.declaration();
         opDecl: OperationDeclContext <- Option(decl.operationDecl())) {
      val name = opDecl.name.getText
      datatypeConstructors +:= FuncDecl(
        name = opDecl.name.getText,
        arguments = opDecl.params.toList.map(transformVariable),
        resultType = SimpleType("operation"),
        attributes = List(Attribute("constructor"))
      )
    }


    // add custom query functions
    for (decl: DeclarationContext <- programContext.declaration();
         query: QueryDeclContext <- Option(decl.queryDecl())) {
      val name = query.name.getText
      queryFunctions += (name -> FuncDecl(
        name = name,
        arguments = query.params.toList.map(transformVariable) ++ stateVars.map(g => VarDecl(g.name, g.typ)),
        resultType = transformTypeExpr(query.returnType),
        implementation = Some(transformExpr(query.expr()))
      ))
    }


    // add invariants
    invariants = for (decl: DeclarationContext <- programContext.declaration().toList;
         inv: InvariantContext <- Option(decl.invariant())) yield {
      transformExpr(inv.expr())
    }

    val standardProcedures = List(
      makeProcBeginAtomic(),
      makeProcEndAtomic(),
      makeProcCrdtOperation()
    )

    val translatedProcedures = for (decl: DeclarationContext <- programContext.declaration();
                                    procedure: ProcedureContext <- Option(decl.procedure())) yield {
      println(s"transform procedure ${procedure.name.getText}")
      transformProcedure(procedure)
    }


    Program(List()
      ++ types.values
      ++ datatypeConstructors
      ++ stateVars
      ++ queryFunctions.values
      ++ List(makeFunc_WellFormed())
      ++ standardProcedures
      ++ translatedProcedures)
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


    Procedure(
      name = "crdtOperation",
      inParams = List("operation" :: typeOperation),
      outParams = List(),
      requires = List(),
      modifies = List("state_callOps", "state_happensBefore", "state_visibleCalls", "state_sameTransaction", "state_currentTransaction"),
      ensures = List(
        Ensures(isFree = true,
          Exists("c" :: typeCallId,
            (Old("state_callOps".get("c")) === ("noop" $()))
              && ("state_callOps".get("c") === "operation")
              && Forall("c1" :: typeCallId, ("c1" !== "c") ==>  ("state_callOps".get("c1") === Old("state_callOps".get("c1"))))
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

  def makeFunc_WellFormed(): FuncDecl = {
    FuncDecl(
      name = "WellFormed",
      arguments = stateVars.map(g => VarDecl(g.name, g.typ)),
      resultType = TypeBool(),
      implementation = Some(
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (("state_callOps".get("c1") === ("noop" $())) || ("state_callOps".get("c2") === ("noop" $())))
            ==> !"state_happensBefore".get("c1", "c2")
        )
        && Forall("c" :: typeCallId, "state_visibleCalls".get("c") ==> ("state_callOps".get("c") !== ("noop"$())))
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


    Procedure(
      name = procedure.name.getText,
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
        transformStatement(procedure.body)(Context()))
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
    captureState(context.start, "before commit"),
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
      }
    } else if (e.unaryOperator != null) {
      FunctionCall(e.unaryOperator.getText, List(transformExpr(e.right)))
    } else {
      throw new RuntimeException("unhandled case: " + e.toStringTree(parser))
    }
  }


  def transformFunctioncall(context: FunctionCallContext): FunctionCall = {
    val funcName: String = context.funcname.getText
    var args: List[Expr] = context.args.toList.map(transformExpr)
    if (queryFunctions.contains(funcName)) {
      // add state vars for query-functions
      args ++= stateVars.map(g => IdentifierExpr(g.name))
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
