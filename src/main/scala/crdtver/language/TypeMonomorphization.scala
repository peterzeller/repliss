package crdtver.language

import crdtver.language.TypedAst._
import crdtver.language.crdts.ACrdtInstance


/**
 * Takes a program and monomorphizes all parametric types.
 */
object TypeMonomorphization {

  private class Ctxt(
    val oldProg: InProgram) {

    var newTypes: Map[(String, List[InTypeExpr]), InTypeDecl] = Map()
  }

  def monomorphizeProgram(prog: InProgram): InProgram = {
    implicit val ctxt: Ctxt = new Ctxt(prog)
    val newProcedures = prog.procedures.map(mProcedure)
    val newAxioms = prog.axioms.map(mAxiom)
    val newInvariants = prog.invariants.map(mInvariant)
    val newProgramCrdt = mCrdt(prog.programCrdt)


    prog.copy(
      procedures = newProcedures,
      types = ctxt.newTypes.values.toList,
      axioms = newAxioms,
      invariants = newInvariants,
      programCrdt = newProgramCrdt
    )

  }

  private def mCrdt(crdt: ACrdtInstance)(implicit ctxt: Ctxt): ACrdtInstance = new ACrdtInstance {
    override val operationType: InTypeExpr = mType(crdt.operationType)
    override val queryType: InTypeExpr = mType(crdt.queryType)

    override def queryReturnType(qry: ACrdtInstance.QueryStructure): InTypeExpr = {
      //mType(crdt.queryReturnType(qry))
      // Should not be use, since we already did type checking
      throw new Exception()
    }


    override val queryDefinitions: List[InQueryDecl] = {
      // TODO rewrite query name?
      for (q <- crdt.queryDefinitions()) yield {
        q.copy(
          params = mVars(q.params),
          returnType = mType(q.returnType),
          implementation = q.implementation.map(mExpr),
          ensures = q.ensures.map(mExpr)
        )
      }
    }

    override def additionalDataTypesRec: List[InTypeDecl] = List()
  }

  private def mInvariant(decl: InInvariantDecl)(implicit ctxt: Ctxt): InInvariantDecl = {
    decl.copy(expr = mExpr(decl.expr))
  }

  private def mAxiom(decl: InAxiomDecl)(implicit ctxt: Ctxt): InAxiomDecl = {
    decl.copy(expr = mExpr(decl.expr))
  }

  def substType(subst: Map[String, InTypeExpr])(t: InTypeExpr): InTypeExpr = t match {
    case TypeVarUse(name) =>
      subst.getOrElse(name, throw new Exception(s"unknown TypeVar $name"))
    case st@SimpleType(_, typeArgs) =>
      st.copy(typeArgs = typeArgs.map(substType(subst)))(st.getSource)
    case _ => t
  }

  def mSubstType(subst: Map[String, InTypeExpr])(t: InTypeExpr)(implicit ctxt: Ctxt): InTypeExpr =
    mType(substType(subst)(t))


  def mSubstVar(subst: Map[String, InTypeExpr])(v: InVariable)(implicit ctxt: Ctxt): InVariable = {
    v.copy(typ = mSubstType(subst)(v.typ))
  }

  private def makePostFix(typeArgs: List[InTypeExpr]): String = {
    if (typeArgs.isEmpty) ""
    else  s"_${typeArgs.mkString("_")}"
  }

  private def specializedTypeDecl(origType: InTypeDecl, typeArgs: List[InTypeExpr])(implicit ctxt: Ctxt): InTypeDecl = {
    val key = (origType.name.name, typeArgs)
    ctxt.newTypes.get(key) match {
      case Some(t) => t
      case None =>
        require(origType.typeParameters.length == typeArgs.length)
        val subst: Map[String, InTypeExpr] = origType.typeParameters.map(_.name.name).zip(typeArgs).toMap
        val namePostfix = makePostFix(typeArgs)
        val newName = s"${origType.name}$namePostfix"
        val newT = origType.copy(
          name = origType.name.copy(name = newName),
          typeParameters = List(),
          dataTypeCases = for (dc <- origType.dataTypeCases) yield {
            dc.copy(
              name = dc.name.copy(name = dc.name.name + namePostfix),
              params = dc.params.map(mSubstVar(subst))
            )
          }
        )
        ctxt.newTypes += (key -> newT)
        newT
    }
  }

  private def specializedType(name: String, typeArgs: List[InTypeExpr], source: SourceTrace)(implicit ctxt: Ctxt): InTypeExpr = {
    val origType = ctxt.oldProg.findType(name).getOrElse(
      throw new Exception(s"Could not find original type $name in ${ctxt.oldProg.types.map(_.name.name).mkString(", ")}")
    )
    val newType: InTypeDecl = specializedTypeDecl(origType, typeArgs)
    if (newType.isIdType) {
      IdType(newType.name.name)(source)
    } else {
      SimpleType(newType.name.name, List())(source)
    }
  }

  private def mType(t: InTypeExpr)(implicit ctxt: Ctxt): InTypeExpr = {
    t match {
      case AnyType() => t
      case UnitType() => t
      case BoolType() => t
      case IntType() => t
      case CallIdType() => t
      case CallInfoType() => t
      case InvocationIdType() => t
      case TransactionIdType() => t
      case InvocationInfoType() => t
      case InvocationResultType() => t
      case SomeOperationType() => t
      case OperationType(_) => t
      case ft@FunctionType(argTypes, returnType, _) =>
        ft.copy(argTypes.map(mType), mType(returnType))(ft.getSource)
      case SimpleType(name, typeArgs) =>
        val typeArgs2 = typeArgs.map(mType)
        specializedType(name, typeArgs2, t.getSource)
      case TypeVarUse(_) =>
        throw new RuntimeException(s"Unexpected type variable $t")
      case IdType(name) =>
        specializedType(name, List(), t.getSource)
    }
  }

  private def mVar(vs: InVariable)(implicit ctxt: Ctxt): InVariable = {
    vs.copy(typ = mType(vs.typ))
  }

  private def mVars(vs: List[InVariable])(implicit ctxt: Ctxt): List[InVariable] = {
    vs.map(mVar)
  }

  private def mProcedure(p: TypedAst.InProcedure)(implicit ctxt: Ctxt): InProcedure = {
    p.copy(
      params = mVars(p.params),
      locals = mVars(p.locals),
      returnType = mType(p.returnType),
      body = mStmt(p.body)
    )
  }

  private def mStmt(stmt: InStatement)(implicit ctxt: Ctxt): InStatement = stmt match {
    case s@BlockStmt(_, stmts) =>
      s.copy(stmts = stmts.map(mStmt))
    case s@Atomic(_, body) =>
      s.copy(body = mStmt(body))
    case s@LocalVar(_, variable) =>
      s.copy(variable = mVar(variable))
    case s@IfStmt(_, cond, thenStmt, elseStmt) =>
      s.copy(cond = mExpr(cond), thenStmt = mStmt(thenStmt), elseStmt = mStmt(elseStmt))
    case s@MatchStmt(_, expr, cases) =>
      s.copy(expr = mExpr(expr), cases = cases.map(mCase))
    case s@CrdtCall(_, call) =>
      s.copy(call = mFunctionCall(call))
    case s@Assignment(_, _, expr) =>
      s.copy(expr = mExpr(expr))
    case s: NewIdStmt =>
      s
    case s@ReturnStmt(_, expr, assertions) =>
      s.copy(expr = mExpr(expr), assertions = assertions.map(mAssertion))
    case s: AssertStmt =>
      mAssertion(s)
  }

  private def mAssertion(stmt: AssertStmt)(implicit ctxt: Ctxt): AssertStmt =
    stmt.copy(expr = mExpr(stmt.expr))

  private def mFunctionCall(f: FunctionCall)(implicit ctxt: Ctxt): FunctionCall = {
    val newTypeArgs = f.typeArgs.map(mType)
    val newName = f.functionName.copy(name = f.functionName.name + makePostFix(newTypeArgs))
    val adaptedType = mType(f.typ)
    f.copy(typeArgs = List(), functionName = newName, typ = adaptedType, args = f.args.map(mExpr))
  }

  private def mCase(c: MatchCase)(implicit ctxt: Ctxt): MatchCase = {
    c.copy(pattern = mExpr(c.pattern), statement = mStmt(c.statement))
  }

  private def mExpr(expr: InExpr)(implicit ctxt: Ctxt): InExpr = expr match {
    case v: VarUse => v.copy(typ = mType(v.typ))
    case c: BoolConst => c
    case c: IntConst => c
    case ce: CallExpr =>
      ce match {
        case f: FunctionCall =>
          mFunctionCall(f)
        case f@ApplyBuiltin(_, typ, _, args) =>
          f.copy(typ = mType(typ), args = args.map(mExpr))
      }
    case f: CrdtQuery =>
      f.copy(typ = mType(f.typ), qryOp = mExpr(f.qryOp).asInstanceOf[FunctionCall])
    case q@QuantifierExpr(_, _, vars, expr) =>
      q.copy(vars = mVars(vars), expr = mExpr(expr))
    case i@InAllValidSnapshots(_, expr) =>
      i.copy(expr = mExpr(expr))
  }


}
