package crdtver

import crdtver.WhyAst.{_}
import crdtver.InputAst.{AnyType, ApplyBuiltin, AssertStmt, Atomic, BF_and, BF_equals, BF_getInfo, BF_getOperation, BF_getOrigin, BF_getResult, BF_greater, BF_greaterEq, BF_happensBefore, BF_implies, BF_inCurrentInvoc, BF_isVisible, BF_less, BF_lessEq, BF_not, BF_notEquals, BF_or, BF_sameTransaction, BlockStmt, BoolType, CallIdType, CrdtCall, IdType, InExpr, InProcedure, InProgram, InStatement, InTypeExpr, InVariable, InlineAnnotation, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, MatchStmt, NewIdStmt, OperationType, QuantifierExpr, ReturnStmt, SomeOperationType, SourcePosition, UnknownType, UnresolvedType, VarUse}
import crdtver.parser.LangParser

/**
  *
  *
  * TODO queries do not allow to update visible things
  */
class WhyTranslation(val parser: LangParser) {

  var types: Map[String, TypeDecl] = Map()
  //  var datatypeConstructors: List[FuncDecl] = List()
  var stateVars: List[GlobalVariable] = List()

  var queryFunctions: Map[String, FunDefn] = Map()

  var invariants: List[Term] = List()

  val callId: String = "callId"
  val typeCallId = TypeSymbol(callId)
  val invocationId: String = "invocationId"
  val typeInvocationId = TypeSymbol(invocationId)
  val invocationInfo: String = "invocationInfo"
  val typeInvocationInfo = TypeSymbol(invocationInfo)
  val invocationResult: String = "invocationResult"
  val typeInvocationResult = TypeSymbol(invocationResult)
  val operation: String = "operation"
  val typeOperation = TypeSymbol(operation)

  var newIdTypes: List[String] = List()

//  var operationDefs: List[(String, List[GlobalVariable])] = List()

  var procedures = List[InProcedure]()
  var procedureNames = Set[String]()

  case class Context(
    procedureName: String = "no_procedure",
    procedureArgNames: List[String] = List(),
    isInAtomic: Boolean = false,
    useOldCurrentInvocation: Boolean = false
  )


  val state_callops: String = "state_callOps"

  val state_visiblecalls: String = "state_visibleCalls"

  val state_happensbefore: String = "state_happensBefore"

  val state_sametransaction: String = "state_sameTransaction"

  val state_currenttransaction: String = "state_currentTransaction"

  val state_maxid: String = "state_maxId"

  val state_origin: String = "state_origin"

  val state_invocations: String = "state_invocations"

  val state_invocationResult: String = "state_" + invocationResult

  val state_invocationHappensBefore: String = "state_invocationHappensBefore"

  val CallId: String = "CallId"

  val InvocationId: String = "InvocationId"

  val noInvocation: String = "NoInvocation"

  val NoResult: String = "NoResult"

  val noop: String = "noop"


  def MapType(keyTypes: List[TypeExpression], resultType: TypeExpression): TypeExpression = {
    ???
  }

  def TypeBool(): TypeExpression = {
    ???
  }

  def TypeInt(): TypeExpression = {
    ???
  }

  def transformProgram(origProgramContext: InProgram): Module = {
    val programContext = AtomicTransform.transformProg(origProgramContext)


    procedures = programContext.procedures
    procedureNames = procedures.map(_.name.name).toSet

    stateVars = List(
      GlobalVariable(state_callops, MapType(List(typeCallId), typeOperation)),
      GlobalVariable(state_visiblecalls, MapType(List(typeCallId), TypeBool())),
      GlobalVariable(state_happensbefore, MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable(state_sametransaction, MapType(List(typeCallId, typeCallId), TypeBool())),
      GlobalVariable(state_currenttransaction, MapType(List(typeCallId), TypeBool())),
      GlobalVariable(state_maxid, TypeSymbol("int")),
      GlobalVariable(state_origin, MapType(List(typeCallId), typeInvocationId)),
      GlobalVariable(state_invocations, MapType(List(typeInvocationId), typeInvocationInfo)),
      GlobalVariable(state_invocationResult, MapType(List(typeInvocationId), typeInvocationResult)),
      GlobalVariable(state_invocationHappensBefore, MapType(List(typeInvocationId, typeInvocationId), TypeBool()))
    )



    // generate types

    // callId type
    val callIdType = TypeDecl(
      name = callId,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = List(
          TypeCase(
            name = CallId,
            paramsTypes = List(TypeParam("id", TypeInt()))
          )
        )
      )
    )
    types += (callId -> callIdType)

    // invocationId type
    val invocationIdType = TypeDecl(
      name = invocationId,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = List(
          TypeCase(
            name = InvocationId,
            paramsTypes = List(TypeParam("id", TypeInt()))
          )
        )
      )
    )
    types += (invocationId -> invocationIdType)

    // invocationInfo type
    val invocationInfoCases = for (procedure <- procedures) yield {
      TypeCase(
        name = invocationInfoForProc(procedure.name.name),
        paramsTypes = procedure.params.map(transformVariableToTypeParam)
      )
    }

    val invocationInfoType = TypeDecl(
      name = invocationInfo,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = List(// TODO add cases for other procedures
          TypeCase(
            name = noInvocation,
            paramsTypes = List()
          )
        ) ++ invocationInfoCases
      )
    )

    types += (invocationInfo -> invocationInfoType)

    // invocationResult type
    val invocationResultCases = for (procedure <- procedures) yield {
      val procName: String = procedure.name.name
      val name = invocationInfoForProc(procName)
      val args: List[TypeParam] = procedure.params.map(transformVariableToTypeParam)

      TypeCase(
        name = invocationResForProc(procName),
        paramsTypes = procedure.returnType match {
          case Some(rt) =>
            List(TypeParam("result", transformTypeExpr(rt)))
          case None =>
            List()
        }
      )
    }

    val invocationResultType = TypeDecl(
      name = invocationResult,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = List(// TODO add cases for other procedures
          TypeCase(
            name = NoResult,
            paramsTypes = List()
          )
        ) ++ invocationResultCases
      )
    )
    types += (invocationResult -> invocationResultType)


    // user defined data types:
    for (typeDecl <- programContext.types) {
      val name: String = typeDecl.name.name



      if (typeDecl.dataTypeCases.isEmpty) {
        val t = TypeDecl(
          name = name,
          definition = AbstractType()
        )
        types += (name -> t)

        if (typeDecl.isIdType) {
          // for id types create additional helpers:

          // set of known IDs
          stateVars +:= GlobalVariable(s"state_knownIds_$name", MapType(List(TypeSymbol(name)), TypeBool()))

          // containsId function for operations:
          newIdTypes +:= name
        }
      } else {
        // Datatype
        val dtcases = for (dtCase <- typeDecl.dataTypeCases) yield {
          TypeCase(
            name =  dtCase.name.name,
            paramsTypes = dtCase.params.toList.map(transformVariableToTypeParam)
          )
        }
        val t = TypeDecl(
          name = name,
          definition = AlgebraicType(dtcases)
        )
        types += (name -> t)

      }
    }


    // generate operations

    val operationCases = for (opDecl <- programContext.operations) yield {
      val name = opDecl.name.name

      TypeCase(
        name = name,
        paramsTypes = opDecl.params.map(transformVariableToTypeParam)
      )
    }

    val operationType = TypeDecl(
      name = operation,
      definition = AlgebraicType(
        List(TypeCase(
          name = noop,
          paramsTypes = List()
        )) ++ operationCases
      )
    )
    types += (operation -> operationType)


    implicit val ctxt = Context()

    // add custom query functions
    for (query <- programContext.queries) {
      val name = query.name.name
      queryFunctions += (name -> FuncDecl(
        name = name,
        arguments = query.params.toList.map(transformVariable) ++ stateVars.map(g => VarDecl(g.name, g.typ)),
        resultType = transformTypeExpr(query.returnType),
        implementation = query.implementation.map(transformExpr),
        attributes = if (query.annotations.contains(InlineAnnotation())) List(Attribute("inline")) else List()
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
      makeStartInvocationProcedure(),
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


  def invocationResForProc(procName: String): String = {
    s"${procName}_res"
  }

  def invocationResultForProc(procName: String): String = {
    s"${procName}_result"
  }

  def invocationInfoForProc(procName: String): String = {
    "invocation_" + procName
  }

  def sortTypes(types: Iterable[TypeDecl], constructors: List[FuncDecl]): List[Declaration] = {
    var result = List[Declaration]()

    for (t <- types) {
      result = result ++ List(t) ++ (for (constr <- constructors; if constr.resultType == TypeSymbol(t.name)) yield constr)
    }

    result
  }


  val check_initialState: String = "check_initialState"

  def initialStateProc(): Procedure = {
    Procedure(
      name = check_initialState,
      inParams = List(),
      outParams = List(),
      requires = List(
        Requires(isFree = false,
          Forall("c" :: typeCallId, state_callops.get("c") === noop.$())),
        Requires(isFree = false,
          Forall("c" :: typeCallId, !state_visiblecalls.get("c"))),
        Requires(isFree = false,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), !state_happensbefore.get("c1", "c2"))),
        Requires(isFree = false,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), !state_sametransaction.get("c1", "c2"))),
        Requires(isFree = false,
          Forall("c" :: typeCallId, !state_currenttransaction.get("c"))),
        Requires(isFree = false,
          Forall("i" :: typeInvocationId, state_invocations.get("i") === noInvocation.$())),
        Requires(isFree = false,
          Forall("i" :: typeInvocationId, state_invocationResult.get("i") === NoResult.$())),
        Requires(isFree = false,
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId), !state_invocationHappensBefore.get("i1", "i2")))
      ),
      modifies = List(),
      ensures =
        // well formed history:
        wellformedConditions().map(Ensures(false, _))
          ++ invariants.map(inv => {
          Ensures(isFree = false, inv)
        }),
      body = Block()
    )
  }

  val beginAtomic: String = "beginAtomic"

  val wellFormed: String = "WellFormed"

  def makeProcBeginAtomic(): Procedure = {

    Procedure(
      name = beginAtomic,
      inParams = List(),
      outParams = List(),
      requires = List(),
      modifies = List(IdentifierExpr(state_visiblecalls)),
      ensures = List(
        // well formed history:
        Ensures(isFree = true,
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        // set of visible updates can grow:
        Ensures(isFree = true,
          Forall("c" :: typeCallId, Old(state_visiblecalls.get("c"))
            ==> state_visiblecalls.get("c"))),
        // causally consistent:
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c2") && state_happensbefore.get("c1", "c2"))
              ==> state_visiblecalls.get("c1"))),
        // transaction consistent:
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c1") && state_sametransaction.get("c1", "c2"))
              ==> state_visiblecalls.get("c2")))
      ),
      body = Block()
    )

  }

  val endAtomic: String = "endAtomic"

  def makeProcEndAtomic(): Procedure = {

    // TODO should add operations from current transaction?

    // TODO should check invariant after endAtomic?

    Procedure(
      name = endAtomic,
      inParams = List(),
      outParams = List(),
      requires = invariants.map(Requires(false, _)),
      modifies = List(),
      ensures = List(),
      body = Block()
    )

  }


  val crdtOperation: String = "crdtOperation"

  def makeProcCrdtOperation(): Procedure = {

    val state_maxId: Expr = state_maxid
    val newCallId: Expr = CallId $ (Old(state_maxId) + IntConst(1))

    Procedure(
      name = crdtOperation,
      inParams = List("currentInvocation" :: typeInvocationId, operation :: typeOperation),
      outParams = List(),
      requires = wellformedConditions().map(Requires(false, _)),
      modifies = List(state_callops, state_happensbefore, state_visiblecalls, state_sametransaction, state_currenttransaction, state_maxid, state_origin),
      ensures = List(
        Ensures(isFree = true,
          Old(state_callops.get(newCallId)) === (noop $())),
        Ensures(isFree = true, state_callops.get(newCallId) === operation),
        Ensures(isFree = true, Forall("c1" :: typeCallId, ("c1" !== newCallId) ==> (state_callops.get("c1") === Old(state_callops.get("c1"))))),
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_happensbefore.get("c1", "c2")
              <==> (Old(state_happensbefore.get("c1", "c2"))
              || ((state_visiblecalls.get("c1") || "c1" === "c2") && "c2" === newCallId)))),
        Ensures(isFree = true,
          Forall("c1" :: typeCallId, state_visiblecalls.get("c1")
            <==> (Old(state_visiblecalls.get("c1")) || "c1" === newCallId))),
        // TODO update current transaction and sameTransaction
        // current transaction update:
        Ensures(isFree = true,
          Forall("c" :: typeCallId,
            state_currenttransaction.get("c") <==> (Old(state_currenttransaction.get("c")) || ("c" === newCallId)))),
        Ensures(isFree = true,
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_sametransaction.get("c1", "c2") <==> (Old(state_sametransaction.get("c1", "c2"))
              || (state_currenttransaction.get("c1") && state_currenttransaction.get("c2")))
          )),
        Ensures(isFree = true,
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        // update state_origin
        Ensures(isFree = true,
          Forall("c" :: typeCallId,
            ("c" !== newCallId) ==> (state_origin.get("c") === Old(state_origin.get("c"))))),
        Ensures(isFree = true,
          state_origin.get(newCallId) === "currentInvocation")
      ),
      body = Block()
    )

  }


  val startInvocation: String = "startInvocation"

  val newInvocId: String = "newInvocId"

  def makeStartInvocationProcedure(): Procedure = {


    Procedure(
      name = startInvocation,
      inParams = List("invocation" :: typeInvocationInfo),
      outParams = List(newInvocId :: typeInvocationId),
      requires = wellformedConditions().map(Requires(false, _)),
      modifies = List(state_invocations),
      //      GlobalVariable("state_origin", MapType(List(typeCallId), typeInvocationId)),
      //      GlobalVariable("state_invocations", MapType(List(typeInvocationId), typeInvocationInfo)),
      //      GlobalVariable("state_invocationHappensBefore", MapType(List(typeInvocationId, typeInvocationId), TypeBool()))
      ensures = List(
        // one fresh invocation added:
        Ensures(isFree = true,
          Old(state_invocations.get(newInvocId) === noInvocation.$())),
        Ensures(isFree = true,
          state_invocationResult.get(newInvocId) === NoResult.$()),
        Ensures(isFree = true,
          state_invocations.get(newInvocId) === "invocation"),
        // other invocations unchanged:
        Ensures(isFree = true,
          Forall("i" :: typeInvocationId, ("i" !== newInvocId) ==> (state_invocations.get("i") === Old(state_invocations.get("i")))))
        // new invocation not in hb (TODO move to wellformed)
        //        Ensures(isFree = true,
        //          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("i", "newInvocId")))),
        //        Ensures(isFree = true,
        //          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("newInvocId", "i")))),
        // current invocation: happensBefore
        //        Ensures(isFree = true,
        //          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        //            "state_invocationHappensBefore".get("i1", "i2") === (
        //              // either already in old hb
        //              Old("state_invocationHappensBefore".get("i1", "i2")))))
        // helper: calls from invocations that happened before, also happen before the current one
      ),
      body = Block()
    )
  }

  val finishInvocation: String = "finishInvocation"

  def makeFinishInvocationProcedure(): Procedure = {


    Procedure(
      name = finishInvocation,
      inParams = List(newInvocId :: typeInvocationId, "res" :: typeInvocationResult),
      outParams = List(),
      requires = wellformedConditions().map(Requires(false, _)),
      modifies = List(state_invocationResult, state_invocationHappensBefore),
      //      GlobalVariable("state_origin", MapType(List(typeCallId), typeInvocationId)),
      //      GlobalVariable("state_invocations", MapType(List(typeInvocationId), typeInvocationInfo)),
      //      GlobalVariable("state_invocationHappensBefore", MapType(List(typeInvocationId, typeInvocationId), TypeBool()))
      ensures = List(
        // well formed history:
        Ensures(isFree = true,
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        //        // origin for new calls:
        //        Ensures(isFree = true,
        //          Forall("c" :: typeCallId, Old("state_inCurrentInvocation".get("c")) ==> ("state_origin".get("c") === "newInvocId"))),
        //        // old calls unchanged:
        //        Ensures(isFree = true,
        //          Forall("c" :: typeCallId, (!Old("state_inCurrentInvocation".get("c"))) ==> ("state_origin".get("c") === Old("state_origin".get("c"))))),
        // one fresh invocation added:
        //        Ensures(isFree = true,
        //          Old("state_invocations".get("newInvocId") === "NoInvocation".$())),
        Ensures(isFree = true,
          state_invocationResult.get(newInvocId) === "res"),
        // other invocations unchanged:
        Ensures(isFree = true,
          Forall("i" :: typeInvocationId, ("i" !== newInvocId) ==> (state_invocationResult.get("i") === Old(state_invocationResult.get("i"))))),
        // new invocation not in hb before the call (TODO move to wellformed)
        Ensures(isFree = true,
          Forall("i" :: typeInvocationId, Old(!state_invocationHappensBefore.get("i", newInvocId)))),
        Ensures(isFree = true,
          Forall("i" :: typeInvocationId, Old(!state_invocationHappensBefore.get(newInvocId, "i")))),
        // current invocation calls cleared
        // current invocation: happensBefore
        //        Ensures(isFree = true,
        //          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        //            "state_invocationHappensBefore".get("i1", "i2") === (
        //              // either already in old hb
        //              Old("state_invocationHappensBefore".get("i1", "i2")))))
        // helper: calls from invocations that happened before, also happen before the current one
        Ensures(isFree = true,
          Forall(List("i" :: typeInvocationId, "c1" :: typeCallId, "c2" :: typeCallId),
            (state_invocationHappensBefore.get("i", newInvocId)
              && Old(state_origin.get("c1") === "i")
              && Old(state_origin.get("c2") === newInvocId))
              ==> state_happensbefore.get("c1", "c2")
          )
        ),
        // TODO real version:
        Ensures(isFree = true,
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
            state_invocationHappensBefore.get("i1", "i2") === (
              // either already in old hb
              Old(state_invocationHappensBefore.get("i1", "i2"))
                // or part of the new hb
                || (("i2" === newInvocId)
                && Exists("c" :: typeCallId, Old(state_origin.get("c") === newInvocId))
                && Exists("c" :: typeCallId, state_origin.get("c") === "i1")
                && Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
                ((state_origin.get("c1") === "i1") && Old(state_origin.get("c2") === newInvocId)) ==> state_happensbefore.get("c1", "c2"))))))
      ),
      body = Block()
    )
  }


  def makeFunc_WellFormed(): FuncDecl = {
    val i: Expr = "i"
    val state_maxId: Expr = state_maxid


    FuncDecl(
      name = wellFormed,
      arguments = stateVars.map(g => VarDecl(g.name, g.typ)),
      resultType = TypeBool(),
      implementation = Some(wellformedConditions().reduce(_ && _))
    )
  }

  def wellformedConditions(): List[Expr] = {
    val i: Expr = "i"
    val state_maxId: Expr = state_maxid
    List(
      // no happensBefore relation between non-existing calls
      Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
        ((state_callops.get("c1") === (noop $())) || (state_callops.get("c2") === (noop $())))
          ==> !state_happensbefore.get("c1", "c2")
      ),
      // visible calls are a subset of all calls
      Forall("c" :: typeCallId, state_visiblecalls.get("c") ==> (state_callops.get("c") !== (noop $()))),
      // happensBefore is a partial order (reflexivity, transitivity, antisymmetric)
      Forall("c" :: typeCallId, (state_callops.get("c") !== (noop $())) ==> state_happensbefore.get("c", "c")),
      Forall(List("x" :: typeCallId, "y" :: typeCallId, "z" :: typeCallId),
        (state_happensbefore.get("x", "y") && state_happensbefore.get("y", "z")) ==> state_happensbefore.get("x", "z")),
      Forall(List("x" :: typeCallId, "y" :: typeCallId), (state_happensbefore.get("x", "y") && state_happensbefore.get("y", "x")) ==> ("x" === "y")),
      Forall("i" :: TypeSymbol("int"), (i >= state_maxId) ==> (state_callops.get(CallId.$(i)) === (noop $()))),
      // invocation happens-before of origins implies happens-before of calls
      Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
        ((state_callops.get("c1") !== noop.$())
          && (state_callops.get("c2") !== noop.$())
          && state_invocationHappensBefore.get(state_origin.get("c1"), state_origin.get("c2")))
          ==> state_happensbefore.get("c1", "c2")),
      // no invocation implies no result
      Forall("i" :: typeInvocationId,
        (state_invocations.get("i") === noInvocation.$()) ==> (state_invocationResult.get("i") === NoResult.$())),
      // no result implies not in invocation happens before
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        (state_invocationResult.get("i1") === NoResult.$()) ==> !state_invocationHappensBefore.get("i1", "i2")),
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        (state_invocationResult.get("i1") === NoResult.$()) ==> !state_invocationHappensBefore.get("i2", "i1")),
      // in happens before implies not NoResult
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        state_invocationHappensBefore.get("i1", "i2") ==> (state_invocationResult.get("i1") !== NoResult.$())),
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        state_invocationHappensBefore.get("i1", "i2") ==> (state_invocationResult.get("i2") !== NoResult.$()))
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
      requires =
        // well-formed state
        wellformedConditions().map(Requires(false, _))
          // assume invariants
          ++ invariants.map(Requires(false, _))
      ,
      modifies = stateVars.map(g => IdentifierExpr(g.name)),
      ensures = invariants.map(Ensures(false, _)),
      body = makeBlock(
        transformLocals(procedure.locals),
        makeBlock(transformPatternmatchLocals(procedure.body)),
        LocalVar("new" + InvocationId, typeInvocationId),
        ProcCall(Some("new" + InvocationId), startInvocation, List(FunctionCall(invocationInfoForProc(procname), paramNames)))
          .setTrace(TextTraceInfo("Starting new invocation")),
        // call endAtomic to check invariants (TODO make extra procedure to check invariants)
        ProcCall(None, endAtomic, List())
          .setTrace(TextTraceInfo("Checking invariants at procedure start")),
        captureState(procedure, s"start of procedure $procname"),
        transformStatement(procedure.body),
        if (procedure.returnType.isEmpty) {
          makeReturn(None, List(), procedure)
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
    case ReturnStmt(source, expr, _) =>
      List()
    case _: AssertStmt =>
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

  def transformVariableToTypeParam(variable: InVariable): TypeParam = ???


  def transformVariable(variable: InVariable): VarDecl =
    VarDecl(variable.name.name, transformTypeExpr(variable.typ))


  def transformBlockStmt(context: BlockStmt)(implicit ctxt: Context): Statement = {
    makeBlock(context.stmts.toList.map(transformStatement))
  }

  def transformAtomicStmt(context: Atomic)(implicit ctxt: Context): Statement = makeBlock(
    ProcCall(None, beginAtomic, List()),
    captureState(context, "begin atomic"),
    transformStatement(context.body)(ctxt.copy(isInAtomic = true)),
    captureState(context, "before commit"),
    ProcCall(None, endAtomic, List()).setTrace(EndAtomicTraceInfo(context)),
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
    ProcCall(None, crdtOperation, List("new" + InvocationId, transformFunctioncall(context.call)))
  }

  def transformAssignment(context: InputAst.Assignment)(implicit ctxt: Context): Statement = {
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
    case b@BlockStmt(source, stmts) =>
      transformBlockStmt(b)
    case a@Atomic(source, body) =>
      transformAtomicStmt(a)
    case l@InputAst.LocalVar(source, variable) =>
      // was already translated at beginning of procedure
      makeBlock()
    case i@InputAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      transformIfStmt(i)
    case m@InputAst.MatchStmt(source, expr, cases) =>
      transformMatchStmt(m)
    case c@CrdtCall(source, call) =>
      transformCrdtCall(c)
    case a@InputAst.Assignment(source, varname, expr) =>
      transformAssignment(a)
    case n@NewIdStmt(source, varname, typename) =>
      transformNewIdStmt(n)
    case r: ReturnStmt =>
      transformReturnStmt(r)
    case AssertStmt(source, expr) =>
      BoogieAst.Assert(transformExpr(expr))
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


  def transformExpr(e: InExpr)(implicit ctxt: Context): Expr = {
    val res = e match {
      case VarUse(source, typ, name) =>
        IdentifierExpr(name)
      case fc@InputAst.FunctionCall(source, typ, functionName, args) =>
        transformFunctioncall(fc)
      case ab@ApplyBuiltin(source, typ, function, args) =>
        transformApplyBuiltin(ab)
      case qe@QuantifierExpr(source, typ, quantifier, vars, expr) =>
        transformQuantifierExpr(qe)
    }
    res.setTrace(AstElementTraceInfo(e))
  }

  def transformApplyBuiltin(ab: ApplyBuiltin)(implicit ctxt: Context): Expr = {
    val args = ab.args.map(transformExpr)
    ab.function match {
      case BF_happensBefore() =>
        if (ab.args.head.getTyp.isSubtypeOf(InvocationIdType())) {
          Lookup(state_invocationHappensBefore, args)
        } else {
          Lookup(state_happensbefore, args)
        }
      case BF_sameTransaction() =>
        Lookup(state_sametransaction, args)
      case BF_isVisible() =>
        Lookup(state_visiblecalls, args)
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
        Lookup(state_callops, args)
      case BF_getInfo() =>
        Lookup(state_invocations, args)
      case BF_getResult() =>
        Lookup(state_invocationResult, args)
      case BF_getOrigin() =>
        Lookup(state_origin, args)
      case BF_inCurrentInvoc() =>
        Lookup(state_origin, args) === "new" + InvocationId
      //        Lookup("old_state_inCurrentInvocation" else "state_inCurrentInvocation", args)
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
    for ((opName, args2) <- operationDefs) {
      val args = args2.map(v => v.copy(name = "_p_" + v.name))
      val idType = TypeSymbol(typeName)
      val argIds: List[IdentifierExpr] = args.map(a => IdentifierExpr(a.name))
      result = result ++ (for (arg <- args; if arg.typ == idType) yield {
        Assume(Forall(("c" :: typeCallId) +: args, (state_callops.get("c") === FunctionCall(opName, argIds)) ==> (IdentifierExpr(idName) !== arg.name)))
      })
    }
    result
  }

  def transformReturnStmt(context: InputAst.ReturnStmt)(implicit ctxt: Context): Statement = {
    val returnedExpr: Expr = transformExpr(context.expr)
    makeReturn(Some(returnedExpr), context.assertions, context)
  }


  def makeReturn(returnedExpr: Option[Expr], endAssertions: List[AssertStmt], source: InputAst.AstElem)(implicit ctxt: Context): Statement = {
    val procRes = FunctionCall(ctxt.procedureName + "_res", returnedExpr.toList)

    makeBlock(
      if (ctxt.isInAtomic) {
        ProcCall(None, endAtomic, List()).setTrace(EndAtomicTraceInfo(source))
      } else {
        makeBlock()
      },
      captureState(source, s"before return"),
      ProcCall(None, finishInvocation, List("new" + InvocationId, procRes)),
      makeBlock(
        endAssertions.map(transformStatement(_)(ctxt.copy(useOldCurrentInvocation = true))) // TODO add old current invocation
      ),
      returnedExpr match {
        case None => makeBlock()
        case Some(e) => Return(e)
      }
    )
  }

  def transformFunctioncall(context: InputAst.FunctionCall)(implicit ctxt: Context): BoogieAst.FunctionCall = {
    var funcName: String = context.functionName.name
    var args: List[Expr] = context.args.toList.map(transformExpr)
    if (queryFunctions.contains(funcName)) {
      // add state vars for query-functions
      args ++= stateVars.map(g => IdentifierExpr(g.name))
    }
    if (procedureNames.contains(funcName)) {
      // add invocation name
      funcName = invocationInfoForProc(funcName)
    }
    FunctionCall(funcName, args)
  }

  def transformQuantifierExpr(q: InputAst.QuantifierExpr)(implicit ctxt: Context): Expr = {
    val vars = q.vars.toList.map(transformVariable)
    val e = transformExpr(q.expr)
    q.quantifier match {
      case InputAst.Forall() => Forall(vars, e)
      case InputAst.Exists() => Exists(vars, e)
    }
  }

  def transformTypeExpr(t: Option[InTypeExpr]): Option[TypeExpression] = t.map(transformTypeExpr)

  def transformTypeExpr(t: InTypeExpr): TypeExpression = t match {
    case AnyType() => ???
    case UnknownType() => ???
    case BoolType() => TypeBool()
    case IntType() => TypeSymbol("int")
    case CallIdType() => TypeSymbol(callId)
    case InvocationIdType() => TypeSymbol(invocationId)
    case InvocationInfoType() => TypeSymbol(invocationInfo)
    case InvocationResultType() => TypeSymbol(invocationResult)
    case SomeOperationType() => TypeSymbol(operation)
    case OperationType(name, source) => TypeSymbol(operation)
    case InputAst.FunctionType(argTypes, returnType, source) => ???
    case InputAst.TypeSymbol(name, source) => TypeSymbol(name)
    case IdType(name, source) => TypeSymbol(name)
    case UnresolvedType(name, source) =>
      println(s"WARNING unresolved type $name in line ${source.getLine}")
      TypeSymbol(name)
  }

  //  def transformTypeExpr(t: InTypeExpr): TypeExpression = {
  //    val typeName: String = t.name.getText
  //    if (typeName == "Boolean") {
  //      TypeBool()
  //    } else {
  //      TypeSymbol(typeName)
  //    }
  //
  //  }

}
