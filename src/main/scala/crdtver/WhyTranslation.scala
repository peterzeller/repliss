package crdtver

import crdtver.BoogieAst.Havoc
import crdtver.WhyAst.{FunDefn, _}
import crdtver.InputAst.{AnyType, ApplyBuiltin, AssertStmt, Atomic, BF_and, BF_equals, BF_getInfo, BF_getOperation, BF_getOrigin, BF_getResult, BF_greater, BF_greaterEq, BF_happensBefore, BF_implies, BF_inCurrentInvoc, BF_isVisible, BF_less, BF_lessEq, BF_not, BF_notEquals, BF_or, BF_sameTransaction, BlockStmt, BoolType, CallIdType, CrdtCall, FunctionType, IdType, InExpr, InProcedure, InProgram, InStatement, InTypeExpr, InVariable, InlineAnnotation, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, MatchStmt, NewIdStmt, OperationType, QuantifierExpr, ReturnStmt, SimpleType, SomeOperationType, SourcePosition, UnknownType, UnresolvedType, VarUse}
import crdtver.parser.LangParser

/**
  *
  * TODO noninterference check
  *
  */
class WhyTranslation(val parser: LangParser) {

  var types: Map[String, TypeDecl] = Map()
  //  var datatypeConstructors: List[FuncDecl] = List()
  var stateVars: List[GlobalVariable] = List()

  var queryFunctions: Map[String, AbstractFunction] = Map()

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

  var operationDefs: Map[String, List[TypedParam]] = Map()

  var procedures = List[InProcedure]()
  var procedureNames = Set[String]()

  case class Context(
    procedureName: String = "no_procedure",
    procedureArgNames: List[Symbol] = List(),
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

  val noop: String = "Noop"


  def MapType(keyTypes: List[TypeExpression], resultType: TypeExpression): TypeExpression = {
    TypeSymbol(LQualid(List("Map"), "map"), List(TupleType(keyTypes), resultType))
  }

  def TypeBool(): TypeExpression = {
    TypeSymbol("bool")
  }

  def TypeInt(): TypeExpression = {
    TypeSymbol("int")
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
    generateUserDefinedTypes(programContext)
    generateDerivedTypes()


    // generate operations

    val operationCases = for (opDecl <- programContext.operations) yield {
      val name = opDecl.name.name
      val paramTypes: List[TypedParam] = opDecl.params.map(transformVariableToTypeParam)

      operationDefs += (name -> paramTypes)

      TypeCase(
        name = operationCaseName(name),
        paramsTypes = paramTypes
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
      val specs = query.implementation match {
        case Some(impl) =>
          List(
            Ensures("result" === transformExpr(impl))
          )
        case None =>
          List()
        // TODO handle other functions
      }
      queryFunctions += (name ->
        AbstractFunction(
          name = name,
          params = query.params.toList.map(transformVariableToTypeParam) ++ stateVars.map(g => TypedParam(g.name, g.typ)),
          returnType = transformTypeExpr(query.returnType),
          specs = specs
        )
        )

      //        GlobalLet(
      //        name = name,
      //        funBody = FunBody(
      //          params = query.params.toList.map(transformVariableToTypeParam) ++ stateVars.map(g => TypedParam(g.name, g.typ)),
      //          returnType = Some(transformTypeExpr(query.returnType)),
      //          body = query.implementation.map(transformExpr).get // TODO handle functions without body
      //        )
      //        arguments =
      //        resultType = transformTypeExpr(query.returnType),
      //        implementation = query.implementation.map(transformExpr),
      //        attributes = if (query.annotations.contains(InlineAnnotation())) List(Attribute("inline")) else List()
      //      ))
    }


    // add invariants
    invariants = for (inv <- programContext.invariants) yield {
      transformExpr(inv.expr).setTrace(AstElementTraceInfo(inv))
    }

    val standardProcedures: List[MDecl] = List(
      makeProcBeginAtomic(),
      makeProcEndAtomic(),
      makeProcCrdtOperation(),
      makeStartInvocationProcedure(),
      makeFinishInvocationProcedure()
    )

    val translatedProcedures: List[GlobalLet] = for (procedure <- procedures) yield {
      transformProcedure(procedure)
    }

    val axioms = for (axiom <- programContext.axioms) yield {
      Axiom(
        name = lIdent("someAxiom"),
        formula = Forall(stateVars.map(g => TypedParam(g.name, g.typ)), transformExpr(axiom.expr)))
    }

    val imports = List(
      Import(false, ImpExpImport(), TQualid(List[LIdent]("map"), "Map")),
      Import(false, ImpExpImport(), TQualid(List[LIdent]("int"), "Int"))
    )

    Module(
      name = "CrdtProgram",
      labels = List(),
      declarations = List()
        ++ imports
        ++ types.values.map(d => TypeDecls(List(d))) // List(TypeDecls(types.values.toList))
        ++ stateVars
        ++ queryFunctions.values
        ++ axioms
        ++ List(makeFunc_WellFormed())
        ++ standardProcedures
        ++ List(initialStateProc())
        ++ translatedProcedures
    )
    //    Program(List()
    //      ++ types.values
    //      ++ stateVars
    //      ++ queryFunctions.values
    //      ++ axioms
    //      ++ List(makeFunc_WellFormed())
    //      ++ standardProcedures
    //      ++ List(initialStateProc())
    //      ++ translatedProcedures)
  }


  def operationCaseName(name: String): String = {
    "Op_" + name
  }

  def typeName(name: String): String = name.charAt(0).toLower + name.substring(1)

  def generateUserDefinedTypes(programContext: InProgram): Unit = {
    // user defined data types:
    for (typeDecl <- programContext.types) {
      val name: String = typeName(typeDecl.name.name)



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
            name = dtCase.name.name.capitalize,
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
  }

  def generateDerivedTypes(): Unit = {
    // callId type
    val callIdType = TypeDecl(
      name = callId,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = List(
          TypeCase(
            name = CallId,
            paramsTypes = List(TypedParam("id", TypeInt()))
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
            paramsTypes = List(TypedParam("id", TypeInt()))
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
      val args: List[TypedParam] = procedure.params.map(transformVariableToTypeParam)

      TypeCase(
        name = invocationResForProc(procName),
        paramsTypes = procedure.returnType match {
          case Some(rt) =>
            List(TypedParam("result", transformTypeExpr(rt)))
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
  }

  def invocationResForProc(procName: String): String = {
    s"${procName.capitalize}_res"
  }

  def invocationResultForProc(procName: String): String = {
    s"${procName.capitalize}_result"
  }

  def invocationInfoForProc(procName: String): String = {
    "Invocation_" + procName
  }

  //  def sortTypes(types: Iterable[TypeDecl], constructors: List[FuncDecl]): List[Declaration] = {
  //    var result = List[Declaration]()
  //
  //    for (t <- types) {
  //      result = result ++ List(t) ++ (for (constr <- constructors; if constr.resultType == TypeSymbol(t.name)) yield constr)
  //    }
  //
  //    result
  //  }


  val check_initialState: String = "check_initialState"

  /**
    * a procedure to check if the initial state satisfies all invariants
    */
  def initialStateProc(): GlobalLetRec = {
    GlobalLetRec(List(
      FunDefn(
        isGhost = false,
        name = check_initialState,
        labels = List(),
        body = FunBody(
          params = List(),
          returnType = Some(unitType()),
          specs = List(
            Requires(
              Forall("c" :: typeCallId, state_callops.get("c") === noop.$())),
            Requires(
              Forall("c" :: typeCallId, !state_visiblecalls.get("c"))),
            Requires(
              Forall(List("c1" :: typeCallId, "c2" :: typeCallId), !state_happensbefore.get("c1", "c2"))),
            Requires(
              Forall(List("c1" :: typeCallId, "c2" :: typeCallId), !state_sametransaction.get("c1", "c2"))),
            Requires(
              Forall("c" :: typeCallId, !state_currenttransaction.get("c"))),
            Requires(
              Forall("i" :: typeInvocationId, state_invocations.get("i") === noInvocation.$())),
            Requires(
              Forall("i" :: typeInvocationId, state_invocationResult.get("i") === NoResult.$())),
            Requires(
              Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
                !state_invocationHappensBefore.get("i1", "i2"))))
            ++ wellformedConditions().map(Ensures(_))
            ++ invariants.map(inv => Ensures(inv)),
          otherSpecs = List(),
          body = Assert(BoolConst(true))
        )
      )
    ))
  }

  val beginAtomic: String = "beginAtomic"

  val wellFormed: String = "wellFormed"

  /**
    * procedure to start a transaction
    */
  def makeProcBeginAtomic(): AbstractFunction = {

    AbstractFunction(
      name = beginAtomic,
      params = List(),
      returnType = unitType(),
      specs = List(
        Writes(List(state_visiblecalls)),
        // well formed history:
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => Symbol(g.name)))),
        // set of visible updates can grow:
        Ensures(
          Forall("c" :: typeCallId, Old(state_visiblecalls.get("c"))
            ==> state_visiblecalls.get("c"))),
        // causally consistent:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c2") && state_happensbefore.get("c1", "c2"))
              ==> state_visiblecalls.get("c1"))),
        // transaction consistent:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c1") && state_sametransaction.get("c1", "c2"))
              ==> state_visiblecalls.get("c2"))))

    )
  }

  val endAtomic: String = "endAtomic"

  /**
    * procedure to end a transaction.
    * at the end of a transaction we check the invariants
    */
  def makeProcEndAtomic(): AbstractFunction = {

    // TODO should add operations from current transaction?

    // TODO should check invariant after endAtomic?

    AbstractFunction(
      name = endAtomic,
      params = List(),
      returnType = unitType(),
      specs =
        invariants.map(Requires)
    )
  }


  val crdtOperation: String = "crdtOperation"

  /**
    * a procedure to execute a CRDT operation
    */
  def makeProcCrdtOperation(): AbstractFunction = {

    val state_maxId: Expr = state_maxid
    val newCallId: Expr = CallId $ (Old(state_maxId) + IntConst(1))

    AbstractFunction(
      name = crdtOperation,
      params = List("currentInvocation" :: typeInvocationId, operation :: typeOperation),
      returnType = unitType(),
      specs = List(
        Writes(List(state_callops, state_happensbefore, state_visiblecalls, state_sametransaction, state_currenttransaction, state_maxid, state_origin)),
        Ensures(
          Old(state_callops.get(newCallId)) === (noop $())),
        Ensures(state_callops.get(newCallId) === operation),
        Ensures(Forall("c1" :: typeCallId, ("c1" !== newCallId) ==> (state_callops.get("c1") === Old(state_callops.get("c1"))))),
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_happensbefore.get("c1", "c2")
              <==> (Old(state_happensbefore.get("c1", "c2"))
              || ((state_visiblecalls.get("c1") || "c1" === "c2") && "c2" === newCallId)))),
        Ensures(
          Forall("c1" :: typeCallId, state_visiblecalls.get("c1")
            <==> (Old(state_visiblecalls.get("c1")) || "c1" === newCallId))),
        // TODO update current transaction and sameTransaction
        // current transaction update:
        Ensures(
          Forall("c" :: typeCallId,
            state_currenttransaction.get("c") <==> (Old(state_currenttransaction.get("c")) || ("c" === newCallId)))),
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_sametransaction.get("c1", "c2") <==> (Old(state_sametransaction.get("c1", "c2"))
              || (state_currenttransaction.get("c1") && state_currenttransaction.get("c2")))
          )),
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        // update state_origin
        Ensures(
          Forall("c" :: typeCallId,
            ("c" !== newCallId) ==> (state_origin.get("c") === Old(state_origin.get("c"))))),
        Ensures(
          state_origin.get(newCallId) === "currentInvocation")
      )

    )
  }


  val startInvocation: String = "startInvocation"

  val newInvocId: String = "newInvocId"

  /**
    * a procedure used at the start of each invocation to setup the local state etc.
    */
  def makeStartInvocationProcedure(): AbstractFunction = {
    AbstractFunction(
      name = startInvocation,
      params = List("invocation" :: typeInvocationInfo),
      returnType = typeInvocationId,
      specs = List(
        Writes(List(state_invocations)),
        // one fresh invocation added:
        Ensures(
          Old(state_invocations.get(newInvocId) === noInvocation.$())),
        Ensures(
          state_invocationResult.get(newInvocId) === NoResult.$()),
        Ensures(
          state_invocations.get(newInvocId) === "invocation"),
        // other invocations unchanged:
        Ensures(
          Forall("i" :: typeInvocationId, ("i" !== newInvocId) ==> (state_invocations.get("i") === Old(state_invocations.get("i")))))
        // new invocation not in hb (TODO move to wellformed)
        //        Ensures(
        //          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("i", "newInvocId")))),
        //        Ensures(
        //          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("newInvocId", "i")))),
        // current invocation: happensBefore
        //        Ensures(
        //          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        //            "state_invocationHappensBefore".get("i1", "i2") === (
        //              // either already in old hb
        //              Old("state_invocationHappensBefore".get("i1", "i2")))))
        // helper: calls from invocations that happened before, also happen before the current one
      )
    )
  }

  val finishInvocation: String = "finishInvocation"

  /**
    * a procedure used at the end of each invocation
    */
  def makeFinishInvocationProcedure(): AbstractFunction = {
    AbstractFunction(
      name = finishInvocation,
      params = List(newInvocId :: typeInvocationId, "res" :: typeInvocationResult),
      returnType = unitType(),
      specs = List(
        Writes(List(state_invocationResult, state_invocationHappensBefore)),
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        //        // origin for new calls:
        //        Ensures(
        //          Forall("c" :: typeCallId, Old("state_inCurrentInvocation".get("c")) ==> ("state_origin".get("c") === "newInvocId"))),
        //        // old calls unchanged:
        //        Ensures(
        //          Forall("c" :: typeCallId, (!Old("state_inCurrentInvocation".get("c"))) ==> ("state_origin".get("c") === Old("state_origin".get("c"))))),
        // one fresh invocation added:
        //        Ensures(
        //          Old("state_invocations".get("newInvocId") === "NoInvocation".$())),
        Ensures(
          state_invocationResult.get(newInvocId) === "res"),
        // other invocations unchanged:
        Ensures(
          Forall("i" :: typeInvocationId, ("i" !== newInvocId) ==> (state_invocationResult.get("i") === Old(state_invocationResult.get("i"))))),
        // new invocation not in hb before the call (TODO move to wellformed)
        Ensures(
          Forall("i" :: typeInvocationId, Old(!state_invocationHappensBefore.get("i", newInvocId)))),
        Ensures(
          Forall("i" :: typeInvocationId, Old(!state_invocationHappensBefore.get(newInvocId, "i")))),
        // current invocation calls cleared
        // current invocation: happensBefore
        //        Ensures(
        //          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        //            "state_invocationHappensBefore".get("i1", "i2") === (
        //              // either already in old hb
        //              Old("state_invocationHappensBefore".get("i1", "i2")))))
        // helper: calls from invocations that happened before, also happen before the current one
        Ensures(
          Forall(List("i" :: typeInvocationId, "c1" :: typeCallId, "c2" :: typeCallId),
            (state_invocationHappensBefore.get("i", newInvocId)
              && Old(state_origin.get("c1") === "i")
              && Old(state_origin.get("c2") === newInvocId))
              ==> state_happensbefore.get("c1", "c2")
          )
        ),
        // TODO real version:
        Ensures(
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
      )
    )
  }

  /**
    * a function that takes all state vars and checks whether the state is well-formed
    */
  def makeFunc_WellFormed(): AbstractFunction = {
    val i: Expr = "i"
    val state_maxId: Expr = state_maxid
    val body = wellformedConditions().reduce(_ && _)
    AbstractFunction(
      name = wellFormed,
      params = stateVars.map(g => g.name :: g.typ),
      returnType = TypeBool(),
      specs = List(Ensures("result" === body))
    )
  }

  /**
    * the conditions required to check well-formedness
    */
  def wellformedConditions(): List[Term] = {
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


  //  def transformLocals(body: StmtContext): Term = {
  //    var locals = List[Term]()
  //    val listener = new LangBaseVisitor[Unit] {
  //      override def visitLocalVar(lv: LangParser.LocalVarContext): Unit = {
  //        locals +:= transformLocalVar(lv)
  //      }
  //    }
  //    body.accept(listener)
  //    makeBlock(locals)
  //  }

  /**
    * returns the default value for the given type
    *
    * (used to init refs)
    */
  def defaultValue(typ: InTypeExpr): Term = {
    typ match {
      case AnyType() =>
      case UnknownType() =>
      case BoolType() =>
        return BoolConst(false)
      case IntType() =>
        return IntConst(0)
      case CallIdType() =>
      case InvocationIdType() =>
      case InvocationInfoType() =>
      case InvocationResultType() =>
      case SomeOperationType() =>
      case OperationType(name, source) =>
      case FunctionType(argTypes, returnType, source) =>
      case SimpleType(name, source) =>
      case IdType(name, source) =>
      case UnresolvedType(name, source) =>
    }
    FunctionCall(havoc, List()) // TODO create havoc function

  }

  def havoc = "havoc"

  /**
    * havoc function;
    * nondeterministically returns a value
    */
  def havocFunction(): AbstractFunction = {
    AbstractFunction(
      name = havoc,
      params = List(),
      returnType = TypeVariable("a"),
      specs = List()
    )

  }

  /**
    * create let-constructs for local variables around body
    */
  def transformLocals(vars: List[InVariable])(body: Term): Term = {
    vars.foldRight(body)((v, b) => {
      LetTerm(
        pattern = VariablePattern(v.name.name),
        value = FunctionCall("ref", List(defaultValue(v.typ))),
        body = b
      )
    })
  }


  /**
    * Transforms a procedure into a why-function with the
    * pre- and post-conditions that need to be checked
    */
  def transformProcedure(procedure: InProcedure): GlobalLet = {


    val procname: String = procedure.name.name
    val params: List[TypedParam] = procedure.params.map(transformVariable)
    val paramNames: List[Symbol] = params.map(p => IdentifierExpr(p.name))
    implicit val initialContext: Context = Context(
      procedureName = procname,
      procedureArgNames = paramNames
    )


    val body = LetTerm(
      pattern = VariablePattern("new" + InvocationId),
      value = FunctionCall(startInvocation, List(FunctionCall(invocationInfoForProc(procname), paramNames))),
      body = makeBlock(
        // call endAtomic to check invariants (TODO make extra procedure to check invariants)
        FunctionCall(endAtomic, List()),
        // execute procedure body:
        transformStatement(procedure.body),
        if (procedure.returnType.isEmpty) {
          makeReturn(None, List(), procedure)
        } else {
          makeBlock()
        }
      )
    )

    val bodyWithLocals = transformLocals(procedure.locals)(body)

    GlobalLet(
      name = procname,
      funBody = FunBody(
        params = params,
        returnType = Some(unitType()),
        specs = List()
          ++ wellformedConditions().map(Requires)
          ++ invariants.map(Requires)
          ++ List(Writes(stateVars.map(g => IdentifierExpr(g.name))))
          ++ invariants.map(Ensures),
        body = bodyWithLocals
      )
    )
  }


  //  def localsInPatterns(pattern: InExpr): List[Term] = pattern match {
  //    case VarUse(source, typ, name) =>
  //      List(LocalVar(name, transformTypeExpr(typ)))
  //    case InputAst.FunctionCall(source, typ, functionName, args) =>
  //      args.flatMap(localsInPatterns)
  //    case _ =>
  //      List()
  //  }

  def transformVariableToTypeParam(variable: InVariable): TypedParam =
    TypedParam(variable.name.name, transformTypeExpr(variable.typ))


  def transformVariable(variable: InVariable): TypedParam =
    TypedParam(variable.name.name, transformTypeExpr(variable.typ))


  def transformBlockStmt(context: BlockStmt)(implicit ctxt: Context): Term = {
    makeBlockL(context.stmts.map(transformStatement))
  }

  def transformAtomicStmt(context: Atomic)(implicit ctxt: Context): Term = makeBlock(
    FunctionCall(beginAtomic, List()),
    captureState(context, "begin atomic"),
    transformStatement(context.body)(ctxt.copy(isInAtomic = true)),
    captureState(context, "before commit"),
    FunctionCall(endAtomic, List()).setTrace(EndAtomicTraceInfo(context)),
    captureState(context, "end atomic", context.source.stop)
  )

  //  def transformLocalVar(context: InputAst.InVariable): LocalVar = {
  //    val v = transformVariable(context)
  //    LocalVar(v.name, v.typ)
  //  }


  def transformIfStmt(context: InputAst.IfStmt)(implicit ctxt: Context): Term = {
    Conditional(transformExpr(context.cond),
      transformStatement(context.thenStmt),
      transformStatement(context.elseStmt))
  }


  def transformCrdtCall(context: CrdtCall)(implicit ctxt: Context): Term = {
    FunctionCall(crdtOperation, List("new" + InvocationId, transformFunctioncall(context.call)))
  }

  def transformAssignment(context: InputAst.Assignment)(implicit ctxt: Context): Term = {
    Assignment(context.varname.name, transformExpr(context.expr))
  }

  def transformStatement(stmt: InStatement)(implicit ctxt: Context): Term = {
    if (stmt == null)
      return makeBlock()
    makeBlock(
      captureState(stmt),
      transformStatement2(stmt).setTrace(AstElementTraceInfo(stmt)))
  }

  def captureState(elem: InputAst.AstElem, msg: String = "", psource: SourcePosition = null): Term = {
    makeBlock() // TODO add comment or so
    //    val source = if (psource == null) elem.getSource().start else psource
    //    Assume(BoolConst(true), List(Attribute("captureState", List(Left("[line " + source.line + ":" + source.column + "] " + msg)))))
    //      .setTrace(AstElementTraceInfo(elem))
  }

  def transformStatement2(stmt: InStatement)(implicit ctxt: Context): Term = stmt match {
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
      Assert(transformExpr(expr))
  }


  def transformMatchStmt(m: MatchStmt)(implicit ctxt: Context): Term = {
    val e = transformExpr(m.expr)

    val cases: List[TermCase] =
      for (c <- m.cases) yield {
        TermCase(
          pattern = transformPattern(c.pattern),
          term = transformStatement(c.statement)
        )
      }
    MatchTerm(List(e), cases)
  }

  def transformPattern(p: InExpr): Pattern = {
    ???
  }


  def transformExpr(e: InExpr)(implicit ctxt: Context): Term = {
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
        FunctionCall("=", args)
      case BF_notEquals() =>
        FunctionCall("not", List(FunctionCall("=", args)))
      case BF_and() =>
        FunctionCall("&&", args)
      case BF_or() =>
        FunctionCall("||", args)
      case BF_implies() =>
        FunctionCall("->", args)
      case BF_not() =>
        FunctionCall("not", args)
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

  def transformNewIdStmt(context: NewIdStmt): Term = {
    val varName: String = context.varname.name
    val typeName: String = context.typename.name
    makeBlock(
      // nondeterministic creation of new id
      Havoc(varName),
      // we can assume that the new id was never used in an operation before
      newIdAssumptions(typeName, varName)
    )

  }

  def Havoc(varName: String) = Assignment(varName, FunctionCall(havoc, List()))

  def newIdAssumptions(typeName: String, idName: String): Term = {
    // add axioms for contained ids
    var result = List[Term]()
    for ((opName, args2) <- operationDefs) {
      val args = args2.map(v => v.copy(name = "_p_" + v.name))
      val idType = TypeSymbol(typeName)
      val argIds: List[Symbol] = args.map(a => IdentifierExpr(a.name))
      result = result ++ (for (arg <- args; if arg.typ == idType) yield {
        Assume(Forall(("c" :: typeCallId) +: args, (state_callops.get("c") === FunctionCall(opName, argIds)) ==> (IdentifierExpr(idName) !== IdentifierExpr(arg.name))))
      })
    }
    makeBlockL(result)
  }

  def transformReturnStmt(context: InputAst.ReturnStmt)(implicit ctxt: Context): Term = {
    val returnedExpr: Expr = transformExpr(context.expr)
    makeReturn(Some(returnedExpr), context.assertions, context)
  }


  def makeReturn(returnedExpr: Option[Expr], endAssertions: List[AssertStmt], source: InputAst.AstElem)(implicit ctxt: Context): Term = {
    val procRes = FunctionCall(ctxt.procedureName + "_res", returnedExpr.toList)

    makeBlock(
      if (ctxt.isInAtomic) {
        FunctionCall(endAtomic, List()).setTrace(EndAtomicTraceInfo(source))
      } else {
        makeBlock()
      },
      captureState(source, s"before return"),
      FunctionCall(finishInvocation, List("new" + InvocationId, procRes)),
      makeBlockL(
        endAssertions.map(transformStatement(_)(ctxt.copy(useOldCurrentInvocation = true))) // TODO add old current invocation
      ),
      returnedExpr match {
        case None => Tuple(List())
        case Some(e) => e
      }
    )
  }

  def transformFunctioncall(context: InputAst.FunctionCall)(implicit ctxt: Context): FunctionCall = {
    var funcName: String = context.functionName.name
    var args: List[Expr] = context.args.toList.map(transformExpr)
    if (queryFunctions.contains(funcName)) {
      // add state vars for query-functions
      args ++= stateVars.map(g => IdentifierExpr(g.name))
    } else if (procedureNames.contains(funcName)) {
      // add invocation name
      funcName = invocationInfoForProc(funcName)
    } else if (operationDefs.contains(funcName)) {
      funcName = operationCaseName(funcName)
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
    case InputAst.SimpleType(name, source) => TypeSymbol(typeName(name))
    case IdType(name, source) => TypeSymbol(typeName(name))
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
