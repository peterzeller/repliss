package crdtver.language

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst._
import crdtver.parser.LangParser._
import crdtver.parser.{LangBaseVisitor, LangParser}
import org.antlr.v4.runtime.Token

import scala.jdk.CollectionConverters._

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
      crdts = crdtDecls.map(transformCrdtDecl)
    )
  }

  def transformInvariant(a: InvariantContext): InInvariantDecl = {
    InInvariantDecl(
      source = a,
      name = if (a.name == null) s"" else a.name.getText,
      isFree = a.free != null,
      expr = transformExpr(a.expr())
    )
  }

  def transformAxiom(a: AxiomDeclContext): InAxiomDecl = {
    InAxiomDecl(a, transformExpr(a.expr()))
  }

  def transformOperation(o: OperationDeclContext): InOperationDecl = {
    InOperationDecl(
      source = o,
      name = makeIdentifier(o.name),
      params = o.params.asScala.map(transformVariable).toList
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
      params = o.params.asScala.map(transformVariable).toList,
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
      typeParameters = tansformTypeParameters(t.typeParams()),
      dataTypeCases = t.dataTypeCases.asScala.map(transformDataTypeCase).toList
    )
  }


  def tansformTypeParameters(context: TypeParamsContext): List[TypeParameter] = {
    if (context == null)
      return List()
    else
      context.typeParam().asScala.map(transformTypeParameter).toList
  }

  def transformTypeParameter(context: TypeParamContext): TypeParameter =
    TypeParameter(context, makeIdentifier(context.name))

  def transformDataTypeCase(c: DataTypeCaseContext): DataTypeCase = {
    DataTypeCase(
      source = c,
      name = makeIdentifier(c.name),
      params = c.params.asScala.map(transformVariable).toList
    )
  }


  def makeIdentifier(name: Token): Identifier = {
    Identifier(name, name.getText)
  }

  def transformProcedure(procedure: ProcedureContext): InProcedure = {


    InProcedure(
      source = procedure,
      name = makeIdentifier(procedure.name),
      params = procedure.params.asScala.toList.map(transformVariable),
      locals = transformLocals(procedure.body),
      returnType = Option(procedure.returnType).map(transformTypeExpr).getOrElse(InferType()),
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
    BlockStmt(context, context.stmt().asScala.toList.map(transformStatement))
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
    InKeyDecl(context, makeIdentifier(context.name), transformCrdtType(context.crdttype()))
  }

  def transformKeyDeclList(context: List[KeyDeclContext]): List[InKeyDecl] = {
    context.map(transformKeyDecl)
  }

  def transformCrdtType(crdttype: CrdttypeContext): InCrdtType = {
    if (crdttype.crdt() != null) {
      transformCrdt(crdttype.crdt())
    } else if (crdttype.structcrdt() != null) {
      transformStructCrdt(crdttype.structcrdt())
    } else {
      throw new RuntimeException("unhandled case: " + crdttype.toStringTree())
    }
  }

  def transformCrdtTypeList(context: List[CrdttypeContext]): List[InCrdtType] = {
    context.map(transformCrdtType)
  }

  def transformCrdt(context: CrdtContext): InCrdt = {
    InCrdt(context, makeIdentifier(context.name), transformCrdtTypeList(context.crdttype().asScala.toList))
  }

  def transformStructCrdt(context: StructcrdtContext): InStructCrdt = {
    InStructCrdt(context, transformKeyDeclList(context.keyDecl().asScala.toList))
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
      statement = BlockStmt(context, context.stmt().asScala.toList.map(transformStatement))
    )
  }

  def transformMatchStmt(context: MatchStmtContext): InStatement = {

    MatchStmt(
      source = context,
      expr = transformExpr(context.expr()),
      cases = context.cases.asScala.toList.map(transformMatchCase)
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
    } else if (stmt.assertStmt() != null) {
      transformAssertStmt(stmt.assertStmt())
    } else {
      throw new RuntimeException("unhandled case: " + stmt.toStringTree(LangParser.ruleNames.toList.asJava))
    }
  }

  def transformExpr(e: ExprContext): InExpr = {
    if (e.varname != null) {
      VarUse(e, e.varname.getText)
    } else if (e.boolval != null) {
      val boolval = e.boolval.getText match {
        case "true" => true
        case "false" => false
      }
      BoolConst(e, boolval)
    } else if (e.INT() != null) {
      IntConst(e, Integer.parseInt(e.INT().getText))
    } else if (e.operator != null) {
      e.operator.getText match {
        case "before" =>
          ApplyBuiltin(e, BF_happensBefore(HappensBeforeOn.Unknown()), List(transformExpr(e.left), transformExpr(e.right)))
        case "after" =>
          ApplyBuiltin(e, BF_happensBefore(HappensBeforeOn.Unknown()), List(transformExpr(e.right), transformExpr(e.left)))
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
            case "+" => BF_plus()
            case "-" => BF_minus()
            case "*" => BF_mult()
            case "/" => BF_div()
            case "%" => BF_mod()
          }
          ApplyBuiltin(e, bf, List(transformExpr(e.left), transformExpr(e.right)))
      }
    } else if (e.quantifierExpr() != null) {
      transformQuantifierExpr(e.quantifierExpr())
    } else if (e.functionCall() != null) {
      transformFunctioncall(e.functionCall())
    } else if (e.parenExpr != null) {
      transformExpr(e.parenExpr)
    } else if (e.isAttribute != null) {
      ApplyBuiltin(e, BF_isVisible(), List(transformExpr(e.left)))
    } else if (e.receiver != null) {
      val receiver = transformExpr(e.receiver)
      e.fieldName.getText match {
        case "op" => ApplyBuiltin(e, BF_getOperation(), List(receiver))
        case "info" => ApplyBuiltin(e, BF_getInfo(), List(receiver))
        case "result" => ApplyBuiltin(e, BF_getResult(), List(receiver))
        case "origin" => ApplyBuiltin(e, BF_getOrigin(), List(receiver))
        case "transaction" => ApplyBuiltin(e, BF_getTransaction(), List(receiver))
        case "inCurrentInvocation" => ApplyBuiltin(e, BF_inCurrentInvoc(), List(receiver))
        case other => FunctionCall(e, Identifier(e.fieldName, other), List(receiver))
      }
    } else if (e.unaryOperator != null) {
      ApplyBuiltin(e, BF_not(), List(transformExpr(e.right)))
    } else {
      throw new RuntimeException("unhandled case: " + e.getText)
    }
  }

  def transformNewIdStmt(context: NewIdStmtContext): InStatement = {
    NewIdStmt(context, makeIdentifier(context.varname), UnresolvedType(context.typename.getText, List())())
  }


  def transformReturnStmt(context: ReturnStmtContext): InStatement = {
    ReturnStmt(context, transformExpr(context.expr()), context.assertStmt().asScala.toList.map(transformAssertStmt))
  }

  def transformAssertStmt(context: AssertStmtContext): AssertStmt = {
    AssertStmt(context, transformExpr(context.expr()))
  }

  def transformFunctioncall(context: FunctionCallContext): CallExpr = {
    val args: List[InExpr] = context.args.asScala.toList.map(transformExpr)
    context.funcname.getText match {
      case "sameTransaction" =>
        ApplyBuiltin(context, BF_sameTransaction(), args)
      case _ =>
        FunctionCall(context, makeIdentifier(context.funcname), args)
    }
  }

  def transformQuantifierExpr(q: QuantifierExprContext): InExpr = {
    val vars = q.vars.asScala.toList.map(transformVariable)

    val quantifier = q.quantifier.getText match {
      case "forall" => Forall()
      case "exists" => Exists()
    }

    QuantifierExpr(q, quantifier, vars, transformExpr(q.expr()))
  }


  def transformTypeExpr(t: TypeContext): InTypeExpr = {
    UnresolvedType(t.name.getText, t.typeArgs.asScala.map(transformTypeExpr).toList)(t)
  }

}
