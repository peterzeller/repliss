package crdtver

import crdtver.BoogieAst.{Forall, ProcCall, _}
import crdtver.InputAst.{AnyType, ApplyBuiltin, Atomic, BF_and, BF_equals, BF_getInfo, BF_getOperation, BF_getOrigin, BF_greater, BF_greaterEq, BF_happensBefore, BF_implies, BF_inCurrentInvoc, BF_isVisible, BF_less, BF_lessEq, BF_not, BF_notEquals, BF_or, BlockStmt, BoolType, CallIdType, CrdtCall, IdType, InExpr, InProcedure, InProgram, InStatement, InTypeExpr, InVariable, IntType, InvocationIdType, InvocationInfoType, MatchStmt, NewIdStmt, OperationType, QuantifierExpr, ReturnStmt, SomeOperationType, SourcePosition, SourceTrace, UnknownType, UnresolvedType, VarUse}
import crdtver.parser.LangParser._
import crdtver.parser.{LangBaseVisitor, LangParser}
import org.antlr.v4.runtime.Token

import scala.collection.GenTraversableOnce
import scala.collection.JavaConversions._

/**
  *
  *
  * TODO queries do not allow to update visible things
  */
class BoogieTranslation2(val parser: LangParser) {

  var types: Map[String, TypeDecl] = Map()
  var datatypeConstructors: List[FuncDecl] = List()
  var stateVars: List[GlobalVariable] = List()

  var queryFunctions: Map[String, FuncDecl] = Map()

  var invariants: List[Expr] = List()

  val typeCallId = SimpleType("callId")
  val typeInvocationId = SimpleType("invocationId")
  val typeInvocationInfo = SimpleType("invocationInfo")
  val typeOperation = SimpleType("operation")

  var newIdTypes: List[String] = List()

  var operationDefs: List[(String, List[VarDecl])] = List()

  var procedures = List[InProcedure]()
  var procedureNames = Set[String]()

  case class Context(
    procedureName: String,
    procedureArgNames: List[IdentifierExpr],
    isInAtomic: Boolean = false
  )




  def transformProgram(origProgramContext: InProgram): Program = {
    val programContext = AtomicTransform.transformProg(origProgramContext)


    procedures = programContext.procedures
    procedureNames = procedures.map(_.name.name).toSet

    stateVars = List(
      GlobalVariable("state_callOps", MapType(List(typeCallId), typeOperation)),
      GlobalVariable("state_visibleCalls", MapType(List(typeCallId), TypeBool())),
      GlobalVariable("state_happensBefore", MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable("state_sameTransaction", MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable("state_currentTransaction", MapType(List(typeCallId), TypeBool())),
      GlobalVariable("state_maxId", SimpleType("int")),
      GlobalVariable("state_origin", MapType(List(typeCallId), typeInvocationId)),
      GlobalVariable("state_inCurrentInvocation", MapType(List(typeCallId), TypeBool())),
      GlobalVariable("state_invocations", MapType(List(typeInvocationId), typeInvocationInfo)),
      GlobalVariable("state_invocationHappensBefore", MapType(List(typeInvocationId, typeInvocationId), TypeBool()))
    )



    // generate types

    // callId type
    types += ("callId" -> TypeDecl("callId", List(Attribute("datatype"))))
    datatypeConstructors +:= FuncDecl(
      name = "CallId",
      arguments = List(VarDecl("id", SimpleType("int"))),
      resultType = typeCallId,
      attributes = List(Attribute("constructor"))
    )

    // invocationId type
    types += ("invocationId" -> TypeDecl("invocationId", List(Attribute("datatype"))))
    datatypeConstructors +:= FuncDecl(
      name = "InvocationId",
      arguments = List(VarDecl("id", SimpleType("int"))),
      resultType = typeInvocationId,
      attributes = List(Attribute("constructor"))
    )

    // invocationInfo type
    types += ("invocationInfo" -> TypeDecl("invocationInfo", List(Attribute("datatype"))))

    // add NoInvocation constructor
    datatypeConstructors +:= FuncDecl(
      name = "NoInvocation",
      arguments = List(),
      resultType = typeInvocationInfo,
      attributes = List(Attribute("constructor"))
    )

    // add an invocation constructor for each procedure
    for (procedure <- procedures) {
      val name = "invocation_" + procedure.name.name
      var args: List[VarDecl] =
        procedure.params.map(transformVariable).toList
      procedure.returnType match {
        case Some(rt) =>
          args = args ++ List("result" :: transformTypeExpr(rt))
        case None =>
      }
      datatypeConstructors +:= FuncDecl(
        name = name,
        arguments = args,
        resultType = typeInvocationInfo,
        attributes = List(Attribute("constructor"))
      )
    }



    for (typeDecl <- programContext.types) {
      val name: String = typeDecl.name.name
      val attributes = if (typeDecl.dataTypeCases.isEmpty) List() else List(Attribute("datatype"))
      types += (name -> TypeDecl(name, attributes))

      if (typeDecl.isIdType) {
        // for id types create additional helpers:

        // set of known IDs
        stateVars +:= GlobalVariable(s"state_knownIds_$name", MapType(List(SimpleType(name)), TypeBool()))

        // containsId function for operations:
        newIdTypes +:= name
      }

      for (dtCase <- typeDecl.dataTypeCases) {
        datatypeConstructors +:= FuncDecl(
          name = dtCase.name.name,
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
    for (opDecl <- programContext.operations) {
      val name = opDecl.name.name
      val args: List[VarDecl] = opDecl.params.map(transformVariable)
      datatypeConstructors +:= FuncDecl(
        name = name,
        arguments = args,
        resultType = SimpleType("operation"),
        attributes = List(Attribute("constructor"))
      )

      operationDefs +:= (name, args)


    }


    // add custom query functions
    for (query <- programContext.queries) {
      val name = query.name.name
      queryFunctions += (name -> FuncDecl(
        name = name,
        arguments = query.params.toList.map(transformVariable) ++ stateVars.map(g => VarDecl(g.name, g.typ)),
        resultType = transformTypeExpr(query.returnType),
        implementation = query.implementation.map(transformExpr)
      ))
    }


    // add invariants
    invariants = for (inv <- programContext.invariants) yield {
      transformExpr(inv.expr).setTrace(AstElementTraceInfo(inv))
    }

    val standardProcedures = List(
      makeProcBeginAtomic(),
      makeProcEndAtomic(),
      makeProcCrdtOperation(),
      makeFinishInvocationProcedure()
    )

    val translatedProcedures = for (procedure <- procedures) yield {
      transformProcedure(procedure)
    }

    val axioms = for (axiom <- programContext.axioms) yield {
      Axiom(
        Forall(stateVars.map(g => VarDecl(g.name, g.typ)), transformExpr(axiom.expr)))
    }


    Program(List()
      ++ sortTypes(types.values, datatypeConstructors)
      ++ stateVars
      ++ queryFunctions.values
      ++ axioms
      ++ List(makeFunc_WellFormed())
      ++ standardProcedures
      ++ List(initialStateProc())
      ++ translatedProcedures)
  }




  def sortTypes(types: Iterable[TypeDecl], constructors: List[FuncDecl]): List[Declaration] = {
    var result = List[Declaration]()

    for (t <- types) {
      result = result ++ List(t) ++ (for (constr <- constructors; if constr.resultType == SimpleType(t.name)) yield constr)
    }

    result
  }


  def initialStateProc(): Procedure  = {
    Procedure(
      name = "check_initialState",
      inParams = List(),
      outParams = List(),
      requires = List(
        Requires(isFree = false,
          Forall("c" :: typeCallId, "state_callOps".get("c") === "noop".$())),
        Requires(isFree = false,
          Forall("c" :: typeCallId, !"state_visibleCalls".get("c"))),
        Requires(isFree = false,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), !"state_happensBefore".get("c1","c2"))),
        Requires(isFree = false,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), !"state_sameTransaction".get("c1","c2"))),
        Requires(isFree = false,
          Forall("c" :: typeCallId, !"state_currentTransaction".get("c"))),
        Requires(isFree = false,
          Forall("c" :: typeCallId, !"state_inCurrentInvocation".get("c"))),
        Requires(isFree = false,
          Forall("i" :: typeInvocationId, "state_invocations".get("i") === "NoInvocation".$())),
        Requires(isFree = false,
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId), !"state_invocationHappensBefore".get("i1", "i2")))
      ),
      modifies = List(),
      ensures = List(
        // well formed history:
        Ensures(isFree = false,
          FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name))))
      ) ++ invariants.map(inv => {
        Ensures(isFree = false, inv)
      }),
      body = Block()
    )
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
            ("state_visibleCalls".get("c2") && "state_happensBefore".get("c1", "c2"))
              ==> "state_visibleCalls".get("c1"))),
        // transaction consistent:
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            ("state_visibleCalls".get("c1") && "state_sameTransaction".get("c1", "c2"))
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
      modifies = List("state_callOps", "state_happensBefore", "state_visibleCalls", "state_sameTransaction", "state_currentTransaction", "state_maxId", "state_inCurrentInvocation"),
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
            <==> (Old("state_visibleCalls".get("c1")) || "c1" === newCallId))),
        Ensures(isFree = true,
          Forall("c1" :: typeCallId, "state_inCurrentInvocation".get("c1") === (Old("state_inCurrentInvocation".get("c1") || ("c1" === newCallId)))))
        // TODO update current transaction and sameTransaction
        , Ensures(isFree = true,
          FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name))))
      ),
      body = Block()
    )

  }

  def makeFinishInvocationProcedure(): Procedure = {


    Procedure(
      name = "finishInvocation",
      inParams = List("invocation" :: typeInvocationInfo),
      outParams = List("newInvocId" :: typeInvocationId),
      requires = List(
        Requires(isFree = false, FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name))))
      ),
      modifies = List("state_origin", "state_invocations", "state_invocationHappensBefore", "state_inCurrentInvocation"),
//      GlobalVariable("state_origin", MapType(List(typeCallId), typeInvocationId)),
//      GlobalVariable("state_invocations", MapType(List(typeInvocationId), typeInvocationInfo)),
//      GlobalVariable("state_invocationHappensBefore", MapType(List(typeInvocationId, typeInvocationId), TypeBool()))
      ensures = List(
        // origin for new calls:
        Ensures(isFree = true,
          Forall("c" :: typeCallId, Old("state_inCurrentInvocation".get("c")) ==> ("state_origin".get("c") === "newInvocId"))),
        // old calls unchanged:
        Ensures(isFree = true,
          Forall("c" :: typeCallId, (!Old("state_inCurrentInvocation".get("c"))) ==> ("state_origin".get("c") === Old("state_origin".get("c"))))),
        // one fresh invocation added:
        Ensures(isFree = true,
          Old("state_invocations".get("newInvocId") === "NoInvocation".$())),
        Ensures(isFree = true,
          "state_invocations".get("newInvocId") === "invocation"),
        // other invocations unchanged:
        Ensures(isFree = true,
          Forall("i" :: typeInvocationId, ("i" !== "newInvocId") ==> ("state_invocations".get("i") === Old("state_invocations".get("i"))))),
        // new invocation not in hb (TODO move to wellformed)
        Ensures(isFree = true,
          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("i", "newInvocId")))),
        Ensures(isFree = true,
          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("newInvocId", "i")))),
        // current invocation calls cleared
        Ensures(isFree = true,
          Forall("c" :: typeCallId, !"state_inCurrentInvocation".get("c"))),
        // current invocation: happensBefore
//        Ensures(isFree = true,
//          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
//            "state_invocationHappensBefore".get("i1", "i2") === (
//              // either already in old hb
//              Old("state_invocationHappensBefore".get("i1", "i2")))))
        // TODO real version:
        Ensures(isFree = true,
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
            "state_invocationHappensBefore".get("i1", "i2") === (
              // either already in old hb
              Old("state_invocationHappensBefore".get("i1", "i2"))
              // or part of the new hb
              || (("i2" === "newInvocId")
                && Exists("c" :: typeCallId, Old("state_inCurrentInvocation".get("c")))
                && Exists("c" :: typeCallId, "state_origin".get("c") === "i1")
                && Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
                    (("state_origin".get("c1") === "i1") && Old("state_inCurrentInvocation".get("c2"))) ==> "state_happensBefore".get("c1", "c2"))))))
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
          // invocation happens-before of origins implies happens-before of calls
          && Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
                (("state_callOps".get("c1") !== "noop".$())
                 && ("state_callOps".get("c2") !== "noop".$())
                 && "state_invocationHappensBefore".get("state_origin".get("c1"), "state_origin".get("c2")))
                      ==> "state_happensBefore".get("c1", "c2"))

        // TODO infinitely many free ids
      )
    )
  }


//  def transformLocals(body: StmtContext): Statement = {
//    var locals = List[Statement]()
//    val listener = new LangBaseVisitor[Unit] {
//      override def visitLocalVar(lv: LangParser.LocalVarContext): Unit = {
//        locals +:= transformLocalVar(lv)
//      }
//    }
//    body.accept(listener)
//    makeBlock(locals)
//  }

  def transformLocals(vars: List[InVariable]): Statement = {
    val locals: List[LocalVar] = vars.map(transformLocalVar)
    makeBlock(locals)
  }



  def transformProcedure(procedure: InProcedure): Procedure = {


    val procname: String = procedure.name.name
    val params: List[VarDecl] = procedure.params.map(transformVariable)
    val paramNames: List[IdentifierExpr] = params.map(p => IdentifierExpr(p.name))
    implicit val initialContext: Context = Context(
      procedureName = procname,
      procedureArgNames = paramNames
    )
    Procedure(
      name = procname,
      inParams = params,
      outParams = procedure.returnType match {
        case Some(rt) => List(VarDecl("result", transformTypeExpr(rt)))
        case None => List()
      },
      requires = List(
        // well-formed state
        Requires(isFree = false, FunctionCall("WellFormed", stateVars.map(g => IdentifierExpr(g.name)))),
        // in the beginning there are no calls in the current invocation
        Requires(isFree = false, Forall("c" :: typeCallId, !"state_inCurrentInvocation".get("c")))
      ) // assume invariants
        ++ invariants.map(Requires(false, _))
      ,
      modifies = stateVars.map(g => IdentifierExpr(g.name)),
      ensures = invariants.map(Ensures(false, _)),
      body = makeBlock(
        transformLocals(procedure.locals),
        makeBlock(transformPatternmatchLocals(procedure.body)),
        LocalVar("newInvocationId", typeInvocationId),
        captureState(procedure, s"start of procedure $procname"),
        transformStatement(procedure.body),
        if (procedure.returnType.isEmpty) {
          makeReturn(None, procedure)
        } else {
          makeBlock()
        },
        captureState(procedure, s"end of procedure $procname", procedure.source.stop))
    ).setTrace(AstElementTraceInfo(procedure))
  }

  def transformPatternmatchLocals(s: InStatement): List[Statement] = s match {
    case BlockStmt(source, stmts) =>
      stmts.flatMap(transformPatternmatchLocals)
    case Atomic(source, body) =>
      transformPatternmatchLocals(body)
    case InputAst.LocalVar(source, variable) =>
      List()
    case InputAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      transformPatternmatchLocals(thenStmt) ++ transformPatternmatchLocals(elseStmt)
    case MatchStmt(source, expr, cases) =>
      cases.flatMap(c => localsInPatterns(c.pattern) ++ transformPatternmatchLocals(c.statement))
    case CrdtCall(source, call) =>
      List()
    case InputAst.Assignment(source, varname, expr) =>
      List()
    case NewIdStmt(source, varname, typename) =>
      List()
    case ReturnStmt(source, expr) =>
      List()
  }

  def localsInPatterns(pattern: InExpr): List[Statement] = pattern match {
    case VarUse(source, typ, name) =>
      List(LocalVar(name, transformTypeExpr(typ)))
    case InputAst.FunctionCall(source, typ, functionName, args) =>
      args.flatMap(localsInPatterns)
    case _ =>
      List()
  }


  def transformVariable(variable: InVariable): VarDecl =
    VarDecl(variable.name.name, transformTypeExpr(variable.typ))


  def transformBlockStmt(context: BlockStmt)(implicit ctxt: Context): Statement = {
    makeBlock(context.stmts.toList.map(transformStatement))
  }

  def transformAtomicStmt(context: Atomic)(implicit ctxt: Context): Statement = makeBlock(
    ProcCall(None, "beginAtomic", List()),
    captureState(context, "begin atomic"),
    transformStatement(context.body)(ctxt.copy(isInAtomic = true)),
    captureState(context, "before commit"),
    ProcCall(None, "endAtomic", List()).setTrace(EndAtomicTraceInfo(context)),
    captureState(context, "end atomic", context.source.stop)
  )

  def transformLocalVar(context: InputAst.InVariable): LocalVar = {
    val v = transformVariable(context)
    LocalVar(v.name, v.typ)
  }


  def transformIfStmt(context: InputAst.IfStmt)(implicit ctxt: Context): Statement = {
    IfStmt(transformExpr(context.cond),
      transformStatement(context.thenStmt),
      transformStatement(context.elseStmt))
  }


  def transformCrdtCall(context: CrdtCall)(implicit ctxt: Context): Statement = {
    ProcCall(None, "crdtOperation", List(transformFunctioncall(context.call)))
  }

  def transformAssignment(context: InputAst.Assignment): Statement = {
    Assignment(context.varname.name, transformExpr(context.expr))
  }

  def transformStatement(stmt: InStatement)(implicit ctxt: Context): Statement = {
    if (stmt == null)
      return Block()
    makeBlock(
      captureState(stmt),
      transformStatement2(stmt).setTrace(AstElementTraceInfo(stmt)))
  }

  def captureState(elem: InputAst.AstElem, msg: String = "", psource: SourcePosition = null): Assume = {
    val source = if (psource == null) elem.getSource().start else psource
    Assume(BoolConst(true), List(Attribute("captureState", List(Left("[line " + source.line + ":" + source.column + "] " + msg)))))
      .setTrace(AstElementTraceInfo(elem))
  }

  def transformStatement2(stmt: InStatement)(implicit ctxt: Context): Statement = stmt match {
    case b @ BlockStmt(source, stmts) =>
      transformBlockStmt(b)
    case a @ Atomic(source, body) =>
      transformAtomicStmt(a)
    case l @ InputAst.LocalVar(source, variable) =>
      // was already translated at beginning of procedure
      makeBlock()
    case i @ InputAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      transformIfStmt(i)
    case m @ InputAst.MatchStmt(source, expr, cases) =>
      transformMatchStmt(m)
    case c @ CrdtCall(source, call) =>
      transformCrdtCall(c)
    case a @ InputAst.Assignment(source, varname, expr) =>
      transformAssignment(a)
    case n @ NewIdStmt(source, varname, typename) =>
      transformNewIdStmt(n)
    case r @ ReturnStmt(source, expr) =>
      transformReturnStmt(r)
  }



  def transformMatchStmt(m: MatchStmt)(implicit ctxt: Context): Statement = {
    val e = transformExpr(m.expr)

    val casesTr = NondetIf(
      for (c <- m.cases) yield {
        makeBlock(
          havocPatternVars(c.pattern)
          ++ List(Assume(e === transformExpr(c.pattern)))
          ++ List(transformStatement(c.statement))
        )
      }
    )

    makeBlock(
      // TODO assert some case holds
      casesTr
    )
  }

  def havocPatternVars(pattern: InExpr): List[Statement] = pattern match {
    case VarUse(source, typ, name) => List(Havoc(name))
    case InputAst.FunctionCall(source, typ, functionName, args) =>
      args.flatMap(havocPatternVars)
    case _ =>
      List()
  }


//  def transformStatement2(stmt: InStatement)(implicit ctxt: Context): Statement = {
//    if (stmt.blockStmt() != null) {
//      transformBlockStmt(stmt.blockStmt())
//    } else if (stmt.atomicStmt() != null) {
//      transformAtomicStmt(stmt.atomicStmt())
//    } else if (stmt.localVar() != null) {
//      // transformLocalVar(stmt.localVar())
//      // was already translated at beginning of procedure
//      Block()
//    } else if (stmt.ifStmt() != null) {
//      transformIfStmt(stmt.ifStmt())
//    } else if (stmt.crdtCall() != null) {
//      transofrmCrdtCall(stmt.crdtCall())
//    } else if (stmt.assignment() != null) {
//      transformAssignment(stmt.assignment())
//    } else if (stmt.newIdStmt() != null) {
//      transformNewIdStmt(stmt.newIdStmt())
//    } else if (stmt.returnStmt() != null) {
//      transformReturnStmt(stmt.returnStmt())
//    } else {
//      throw new RuntimeException("unhandled case: " + stmt.toStringTree(parser))
//    }
//  }



  def transformExpr(e: InExpr): Expr = {
    val res = e match {
      case VarUse(source, typ, name) =>
        IdentifierExpr(name)
      case fc @ InputAst.FunctionCall(source, typ, functionName, args) =>
        transformFunctioncall(fc)
      case ab @ ApplyBuiltin(source, typ, function, args) =>
        transformApplyBuiltin(ab)
      case qe @ QuantifierExpr(source, typ, quantifier, vars, expr) =>
        transformQuantifierExpr(qe)
    }
    res.setTrace(AstElementTraceInfo(e))
  }

  def transformApplyBuiltin(ab: ApplyBuiltin): Expr = {
    val args = ab.args.map(transformExpr)
    ab.function match {
      case BF_happensBefore() =>
        if (ab.args.head.getTyp.isSubtypeOf(InvocationIdType())) {
          Lookup("state_invocationHappensBefore", args)
        } else {
          Lookup("state_happensBefore", args)
        }
      case BF_isVisible() =>
        Lookup("state_visibleCalls",args)
      case BF_less() =>
        FunctionCall("<", args)
      case BF_lessEq() =>
        FunctionCall("<=", args)
      case BF_greater() =>
        FunctionCall(">", args)
      case BF_greaterEq() =>
        FunctionCall(">=", args)
      case BF_equals() =>
        FunctionCall("==", args)
      case BF_notEquals() =>
        FunctionCall("!=", args)
      case BF_and() =>
        FunctionCall("&&", args)
      case BF_or() =>
        FunctionCall("||", args)
      case BF_implies() =>
        FunctionCall("==>", args)
      case BF_not() =>
        FunctionCall("!", args)
      case BF_getOperation() =>
        Lookup("state_callOps", args)
      case BF_getInfo() =>
        Lookup("state_invocations", args)
      case BF_getOrigin() =>
        Lookup("state_origin", args)
      case BF_inCurrentInvoc() =>
        Lookup("state_inCurrentInvocation", args)
    }
  }

//  def transformExpr(e: InExpr): Expr = {
//    if (e.varname != null) {
//      IdentifierExpr(e.varname.getText)
//    } else if (e.operator != null) {
//      e.operator.getText match {
//        case "before" =>
//          Lookup("state_happensBefore", List(transformExpr(e.left), transformExpr(e.right)))
//        case "after" =>
//          Lookup("state_happensBefore", List(transformExpr(e.right), transformExpr(e.left)))
//        case op =>
//          FunctionCall(op, List(transformExpr(e.left), transformExpr(e.right)))
//      }
//    } else if (e.quantifierExpr() != null) {
//      transformQuantifierExpr(e.quantifierExpr())
//    } else if (e.functionCall() != null) {
//      transformFunctioncall(e.functionCall())
//    } else if (e.parenExpr != null) {
//      transformExpr(e.parenExpr)
//    } else if (e.isAttribute != null) {
//      Lookup("state_visibleCalls", List(transformExpr(e.left)))
//    } else if (e.receiver != null) {
//      val receiver = transformExpr(e.receiver)
//      e.fieldName.getText match {
//        case "op" => Lookup("state_callOps", List(receiver))
//        case "info" => Lookup("state_invocations", List(receiver))
//        case "origin" => Lookup("state_origin", List(receiver))
//      }
//    } else if (e.unaryOperator != null) {
//      FunctionCall(e.unaryOperator.getText, List(transformExpr(e.right)))
//    } else {
//      throw new RuntimeException("unhandled case: " + e.toStringTree(parser))
//    }
//  }

  def transformNewIdStmt(context: NewIdStmt): Statement = {
    val varName: String = context.varname.name
    val typeName: String = context.typename.name
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
    for ((opName,args2) <- operationDefs) {
      val args = args2.map(v => v.copy(name = "_p_" + v.name))
      val idType = SimpleType(typeName)
      val argIds: List[IdentifierExpr] = args.map(a => IdentifierExpr(a.name))
      result = result ++ (for (arg <- args; if arg.typ == idType) yield {
        Assume(Forall(("c" :: typeCallId) +: args, ("state_callOps".get("c") === FunctionCall(opName, argIds)) ==> (IdentifierExpr(idName) !== arg.name)))
      })
    }
    result
  }

  def transformReturnStmt(context: InputAst.ReturnStmt)(implicit ctxt: Context): Statement = {
    val returnedExpr: Expr = transformExpr(context.expr)
    makeReturn(Some(returnedExpr), context)
  }


  def makeReturn(returnedExpr: Option[Expr], source: InputAst.AstElem)(implicit ctxt: Context): Statement = {
    makeBlock(
      if (ctxt.isInAtomic) {
        ProcCall(None, "endAtomic", List()).setTrace(EndAtomicTraceInfo(source))
      } else {
        makeBlock()
      },
      captureState(source, s"before return"),
      ProcCall(Some("newInvocationId"), "finishInvocation", List(FunctionCall("invocation_" + ctxt.procedureName, ctxt.procedureArgNames ++ returnedExpr.toList))),
      returnedExpr match {
        case None => makeBlock()
        case Some(e) => Return(e)
      }
    )
  }

  def transformFunctioncall(context: InputAst.FunctionCall): BoogieAst.FunctionCall = {
    var funcName: String = context.functionName.name
    var args: List[Expr] = context.args.toList.map(transformExpr)
    if (queryFunctions.contains(funcName)) {
      // add state vars for query-functions
      args ++= stateVars.map(g => IdentifierExpr(g.name))
    }
    if (procedureNames.contains(funcName)) {
      // add invocation name
      funcName = "invocation_" + funcName
    }
    FunctionCall(funcName, args)
  }

  def transformQuantifierExpr(q: InputAst.QuantifierExpr): Expr = {
    val vars = q.vars.toList.map(transformVariable)
    val e = transformExpr(q.expr)
    q.quantifier match {
      case InputAst.Forall() => Forall(vars, e)
      case InputAst.Exists() => Exists(vars, e)
    }
  }

  def transformTypeExpr(t: Option[InTypeExpr]): Option[TypeExpr] = t.map(transformTypeExpr)

  def transformTypeExpr(t: InTypeExpr): TypeExpr = t match {
    case AnyType() => ???
    case UnknownType() => ???
    case BoolType() => TypeBool()
    case IntType() => SimpleType("int")
    case CallIdType() => SimpleType("callId")
    case InvocationIdType() => SimpleType("invocationId")
    case InvocationInfoType() => SimpleType("invocationInfo")
    case SomeOperationType() => SimpleType("operation")
    case OperationType(name, source) => SimpleType("operation")
    case InputAst.FunctionType(argTypes, returnType, source) => ???
    case InputAst.SimpleType(name, source) => SimpleType(name)
    case IdType(name, source) => SimpleType(name)
    case UnresolvedType(name, source) =>
      println(s"WARNING unresolved type $name in line ${source.getLine}")
      SimpleType(name)
  }

//  def transformTypeExpr(t: InTypeExpr): TypeExpr = {
//    val typeName: String = t.name.getText
//    if (typeName == "Boolean") {
//      TypeBool()
//    } else {
//      SimpleType(typeName)
//    }
//
//  }

}
