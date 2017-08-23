package crdtver.language

import InputAst._
import crdtver.language.AntlrAstTransformation.transformExpr
import crdtver.parser.LangParser._
import crdtver.parser.{LangBaseVisitor, LangParser}
import org.antlr.v4.runtime.Token

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._


/**
  * This object is response for transforming the ANTLR syntax tree into the Scala AST defined in InputAst.
  */
object AntlrAstTransformation {

  case class Context(
    programContext: ProgramContext,
    isInAtomic: Boolean = false
  )


  def transformProgram(programName: String, programContext: ProgramContext): InProgram = {
    val procedures = programContext.declaration().asScala.flatMap(d => Option(d.procedure())).toList
    val typeDecls = programContext.declaration().asScala.flatMap(d => Option(d.typedecl())).toList
    val operations = programContext.declaration().asScala.flatMap(d => Option(d.operationDecl())).toList
    val queries = programContext.declaration().asScala.flatMap(d => Option(d.queryDecl())).toList
    val axioms = programContext.declaration().asScala.flatMap(d => Option(d.axiomDecl())).toList
    val invariants = programContext.declaration().asScala.flatMap(d => Option(d.invariant())).toList
    val crdtDecls = programContext.declaration().asScala.flatMap(d => Option(d.crdtDecl())).toList


    implicit val ctxt = Context(programContext)

    InProgram(
      name = programName,
      source = programContext,
      procedures = procedures.map(transformProcedure),
      types = typeDecls.map(transformTypeDecl),
      operations = operations.map(transformOperation),
      queries = queries.map(transformQuery),
      axioms = axioms.map(transformAxiom),
      invariants = invariants.map(transformInvariant),
      crdtDecls = crdtDecls.map(transformCrdtDecl)
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
      implementation = Option(o.implementation).map(transformExpr),
      ensures = Option(o.ensures).map(transformExpr),
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

  def transformCrdtDecl(context: CrdtDeclContext): InCrdtDecl = {
    InCrdtDecl(context, transformKeyDecl(context.keyDecl()))
  }


  def transformKeyDecl(context: KeyDeclContext): InKeyDecl = {
    InKeyDecl(context, makeIdentifier(context.name), transformCrdtType(context.crdttype()) )
  }

  def transformKeyDeclList(context: List[KeyDeclContext]): List[InKeyDecl] = {
    context.map(transformKeyDecl)
  }

  def transformCrdtType(crdttype: CrdttypeContext): InCrdtType = {
    if (crdttype.crdt() != null) {
      transformCrdt(crdttype.crdt())
    } else if (crdttype.mapcrdt() != null) {
      transformMapCrdt(crdttype.mapcrdt())
    } else {
      throw new RuntimeException("unhandled case: " + crdttype.toStringTree())
    }
  }

  def transformCrdt(context: CrdtContext): InCrdt = {
   InCrdt(context, makeIdentifier(context.name), transformTypeExprList(context.`type`().toList))

  }

  def transformMapCrdt(context: MapcrdtContext): InMapCrdt = {
    InMapCrdt(context, transformTypeExpr(context.`type`()), transformKeyDeclList(context.keyDecl().toList))
  }

  def transformTypeExprList(context: List[TypeContext]): List[InTypeExpr] = {
    context.map(transformTypeExpr)
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
