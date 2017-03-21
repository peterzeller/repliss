package crdtver

import crdtver.InputAst.{AnyType, ApplyBuiltin, AssertStmt, Atomic, BF_and, BF_equals, BF_getInfo, BF_getOperation, BF_getOrigin, BF_getResult, BF_greater, BF_greaterEq, BF_happensBefore, BF_implies, BF_inCurrentInvoc, BF_isVisible, BF_less, BF_lessEq, BF_not, BF_notEquals, BF_or, BF_sameTransaction, BlockStmt, BoolType, CallIdType, CrdtCall, FunctionType, IdType, InExpr, InOperationDecl, InProcedure, InProgram, InStatement, InTypeExpr, InVariable, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, MatchStmt, NewIdStmt, OperationType, QuantifierExpr, ReturnStmt, SimpleType, SomeOperationType, SourcePosition, UnknownType, UnresolvedType, VarUse}
import crdtver.WhyAst.{Forall, GlobalVariable, Requires, _}

/**
  *
  * TODO noninterference check
  *
  */
class WhyTranslation {

  private var types: Map[String, TypeDecl] = Map()
  //  var datatypeConstructors: List[FuncDecl] = List()
  private var stateVars: List[GlobalVariable] = List()

  private var queryFunctions: Map[String, LogicDecls] = Map()
  private var queryProcedures: List[AbstractFunction] = List()

  private var functionReplacements = Map[String, String]()

  private var invariants: List[Term] = List()
  private val callId: String = "callId"
  private val typeCallId = TypeSymbol(callId)
  private val invocationId: String = "invocationId"
  private val typeInvocationId = TypeSymbol(invocationId)
  private val invocationInfo: String = "invocationInfo"
  private val typeInvocationInfo = TypeSymbol(invocationInfo)
  private val invocationResult: String = "invocationResult"
  private val typeInvocationResult = TypeSymbol(invocationResult)
  private val operation: String = "operation"
  private val typeOperation = TypeSymbol(operation)

  private var newIdTypes: List[String] = List()

  private var operationDefs: Map[String, List[TypedParam]] = Map()

  private var procedures = List[InProcedure]()
  private var procedureNames = Set[String]()

  private var builtinFuncWrites = Map[String, List[Symbol]]()

  case class Context(
    procedureName: String = "no_procedure",
    procedureArgNames: List[Symbol] = List(),
    isInAtomic: Boolean = false,
    useOldCurrentInvocation: Boolean = false,
    refVars: Set[String] = Set(),
    targetIsLogic: Boolean = true
  ) {
    def isRefVar(varname: String): Boolean = refVars.contains(varname)

  }


  private val state_callops: String = "state_callOps"

  private val state_visiblecalls: String = "state_visibleCalls"

  private val state_happensbefore: String = "state_happensBefore"

  private val state_sametransaction: String = "state_sameTransaction"

  private val state_currenttransaction: String = "state_currentTransaction"

  private val state_origin: String = "state_origin"

  private val state_invocations: String = "state_invocations"

  private val state_invocationResult: String = "state_" + invocationResult

  private val state_invocationHappensBefore: String = "state_invocationHappensBefore"


  // TODO needs to be done per type
  private def state_knownIds(t: String): String = s"state_knownIds_$t"

  private def state_exposed(t: String): String = s"state_exposed_$t"

  private def state_locallyGenerated(t: String): String = s"state_locallyGenerated_${typeName(t)}"

  private val CallId: String = "CallId"

  private val InvocationId: String = "InvocationId"

  private val noInvocation: String = "NoInvocation"

  private val NoResult: String = "NoResult"

  private val noop: String = "Noop"


  def MapType(keyTypes: List[TypeExpression], resultType: TypeExpression): TypeExpression = {
    TypeSymbol(LQualid(List("Map"), "map"), List(TupleType(keyTypes), resultType))
  }

  def ref(t: TypeExpression): TypeExpression = {
    TypeSymbol("ref", List(t))
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
      GlobalVariable(state_callops, ref(MapType(List(typeCallId), typeOperation))),
      GlobalVariable(state_visiblecalls, ref(MapType(List(typeCallId), TypeBool()))),
      GlobalVariable(state_happensbefore, ref(MapType(List(typeCallId, typeCallId), TypeBool()))),
      GlobalVariable(state_sametransaction, ref(MapType(List(typeCallId, typeCallId), TypeBool()))),
      GlobalVariable(state_currenttransaction, ref(MapType(List(typeCallId), TypeBool()))),
      GlobalVariable(state_origin, ref(MapType(List(typeCallId), typeInvocationId))),
      GlobalVariable(state_invocations, ref(MapType(List(typeInvocationId), typeInvocationInfo))),
      GlobalVariable(state_invocationResult, ref(MapType(List(typeInvocationId), typeInvocationResult))),
      GlobalVariable(state_invocationHappensBefore, ref(MapType(List(typeInvocationId, typeInvocationId), TypeBool())))
    )


    // generate types
    generateUserDefinedTypes(programContext)
    generateDerivedTypes()


    // generate operations

    val operationCases = for (opDecl <- programContext.operations) yield {
      val name = opDecl.name.name
      val paramTypes: List[TypedParam] = opDecl.params.map(transformVariableToTypeParam)

      functionReplacements += (name -> operationCaseName(name))
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
      val impl = query.implementation match {
        case Some(impl) =>
          Some(transformExpr(impl))
        case None =>
          None
      }
      val params = query.params.toList.map(transformVariableToTypeParam) ++ stateVars.map(g => TypedParam(g.name, g.typ))
      queryFunctions += (name -> LogicDecls(List(
        LogicDecl(
          name = name,
          params = params,
          returnType = transformTypeExpr(query.returnType),
          implementation = impl
        )
      )))
      queryProcedures :+= AbstractFunction(
        name = s"${name}_proc",
        params = params,
        returnType = transformTypeExpr(query.returnType),
        specs = List(
          Ensures("result" === FunctionCall(name, params.map(p => Symbol(p.name))))
        )
      )
      //      val specs = query.implementation match {
      //        case Some(impl) =>
      //          List(
      //            Ensures("result" === transformExpr(impl))
      //          )
      //        case None =>
      //          List()
      //        // TODO handle other functions
      //      }
      //      queryFunctions += (name ->
      //        AbstractFunction(
      //          name = name,
      //          params = query.params.toList.map(transformVariableToTypeParam) ++ stateVars.map(g => TypedParam(g.name, g.typ)),
      //          returnType = transformTypeExpr(query.returnType),
      //          specs = specs
      //        )
      //        )

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
      Import(false, ImpExpImport(), TQualid(List[LIdent]("ref"), "Ref")),
      Import(false, ImpExpImport(), TQualid(List[LIdent]("int"), "Int"))
    )

    Module(
      name = "CrdtProgram",
      labels = List(),
      declarations = List()
        ++ imports
        ++ types.values.map(d => TypeDecls(List(d))) // List(TypeDecls(types.values.toList))
        ++ stateVars
        ++ newIdTypes.map(containsIdFunc(_, programContext.operations))
        ++ queryFunctions.values
        ++ queryProcedures
        ++ axioms
        ++ List(makeFunc_WellFormed())
        ++ standardProcedures
        ++ List(initialStateProc())
        //        ++ List(mergeStateProc())
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
          stateVars +:= GlobalVariable(state_knownIds(name), ref(MapType(List(TypeSymbol(name)), TypeBool())))
          // exposed(uid, callId) -> bool
          // states that the given uid was part of the arguments in the given callId
          stateVars +:= GlobalVariable(state_exposed(name), ref(MapType(List(TypeSymbol(name), typeCallId), TypeBool())))
          // set of locally generated ids
          stateVars +:= GlobalVariable(state_locallyGenerated(name), ref(MapType(List(TypeSymbol(name)), TypeBool())))


          // containsId function for operations:
          newIdTypes +:= name
        }
      } else {
        // Datatype
        val dtcases = for (dtCase <- typeDecl.dataTypeCases) yield {
          val name = dtCase.name.name.capitalize
          functionReplacements += (dtCase.name.name -> name)
          TypeCase(
            name = name,
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


  def containsId(idType: String): String = s"containsId_$idType"

  def containsIdFunc(idType: String, operations: List[InOperationDecl]): Declaration = {

    // go through all cases and check if id is contained
    val cases = for (opDecl <- operations) yield {
      val name = opDecl.name.name


      var check: Term = BoolConst(false)

      for (arg <- opDecl.params) {
        arg.typ match {
          case t: IdType =>
            println(s"$name: ${typeName(t.name)} == $idType ?")
            if (typeName(t.name) == idType) {
              check = check || (arg.name.name === "idT")
            }
          case _ =>
          // TODO handle nested types like datatypes
        }
      }

      TermCase(
        pattern = ConstructorPattern(
          constructorName = operationCaseName(name),
          args = opDecl.params.map(arg => VariablePattern(arg.name.name))
        ),
        term = check
      )
    }
    //    GlobalLet(
    //      name = containsId(idType),
    //      funBody = FunBody(
    //        params = List(
    //          TypedParam("op", typeOperation),
    //          TypedParam("idT", TypeSymbol(idType))
    //        ),
    //        returnType = Some(TypeBool()),
    //        body = MatchTerm(
    //          terms = List("op"),
    //          cases = cases
    //            ++ List(
    //            TermCase(ConstructorPattern(noop, List()), BoolConst(false))
    //          )
    //        )
    //      )
    //    )
    LogicDecls(
      List(
        LogicDecl(
          name = containsId(idType),
          params = List(
            TypedParam("op", typeOperation),
            TypedParam("idT", TypeSymbol(idType))
          ),
          returnType = TypeBool(),
          implementation = Some(
            MatchTerm(
              terms = List("op"),
              cases = cases
                ++ List(
                TermCase(ConstructorPattern(noop, List()), BoolConst(false))
              )
            )
          )
        )
      )
    )
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

      functionReplacements += (s"${procName}_res" -> invocationResForProc(procName))

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
  def initialStateProc(): GlobalLet = {
    GlobalLet(
      isGhost = false,
      name = check_initialState,
      labels = List(),
      funBody = FunBody(
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
        body = Tuple(List())
      )
    )
  }

  /**
    * a procedure to check if the initial state satisfies all invariants
    */
  def mergeStateProc(): GlobalLet = {

    val s1params = stateVars.map(v => TypedParam(v.name + "_1", v.typ))
    val s2params = stateVars.map(v => TypedParam(v.name + "_2", v.typ))

    GlobalLet(
      isGhost = false,
      name = "check_mergeStates",
      labels = List(),
      funBody = FunBody(
        params = s1params ++ s2params,
        returnType = Some(unitType()),
        specs = List(
          // updates all state vars (or assumes its already updated?)
          // Writes(stateVars.map(v => Symbol(v.name))),
          // state1 and state2 are well-formed:


          //          Requires(
          //            FunctionCall(wellFormed, stateVars.map(g => Symbol(g.name + "_1")))),
          //          Requires(
          //            FunctionCall(wellFormed, stateVars.map(g => Symbol(g.name + "_2")))),
          //          Requires(
          //            FunctionCall(wellFormed, stateVars.map(g => Symbol(g.name))))
        )

          ++ wellformedConditions().map(c => Requires(postfixStateVars(c, "_1")))
          ++ wellformedConditions().map(c => Requires(postfixStateVars(c, "_2")))
          ++ wellformedConditions().map(c => Requires(c))

          // state1 and state2 fulfill the invariant:
          ++ invariants.map(inv => Requires(postfixStateVars(inv, "_1")))
          ++ invariants.map(inv => Requires(postfixStateVars(inv, "_2")))

          ++ List(


          // happens-before is consistent on shared calls
          Requires(Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            ((s"${state_callops}_1".get("c2") !== (noop $()))
              && (s"${state_callops}_2".get("c2") !== (noop $())))
              ==> (s"${state_happensbefore}_1".get("c1", "c2") === s"${state_happensbefore}_2".get("c1", "c2"))
          )),
          // same transaction is consistent on shared calls
          Requires(Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            ((s"${state_callops}_1".get("c2") !== (noop $()))
              && (s"${state_callops}_2".get("c2") !== (noop $())))
              ==> (s"${state_sametransaction}_1".get("c1", "c2") === s"${state_sametransaction}_2".get("c1", "c2"))
          )),

          // callops = union
          Requires(Forall("c" :: typeCallId,
            (s"${state_callops}_1".get("c") !== (noop $()))
              ==> (state_callops.get("c") === s"${state_callops}_1".get("c")))
          ),
          Requires(Forall("c" :: typeCallId,
            (s"${state_callops}_2".get("c") !== (noop $()))
              ==> (state_callops.get("c") === s"${state_callops}_2".get("c")))
          ),
          Requires(Forall("c" :: typeCallId,
            (state_callops.get("c") !== (noop $()))
              ==> ((s"${state_callops}_1".get("c") !== (noop $())) || (s"${state_callops}_2".get("c") !== (noop $())))
          )),
          // visible calls = union (TODO or better to take only s1?)
          Requires(Forall("c" :: typeCallId,
            state_visiblecalls.get("c") === (s"${state_visiblecalls}_1".get("c") || s"${state_visiblecalls}_2".get("c"))
          )),
          // happens before = union
          Requires(Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_happensbefore.get("c1", "c2") === (s"${state_happensbefore}_1".get("c1", "c2") || s"${state_happensbefore}_2".get("c1", "c2"))
          )),
          // same transaction = union
          Requires(Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_sametransaction.get("c1", "c2") === (s"${state_sametransaction}_1".get("c1", "c2") || s"${state_sametransaction}_2".get("c1", "c2"))
          )),
          // current transaction = take from s1
          Requires(state_currenttransaction === s"${state_currenttransaction}_1"),
          // origin = union
          Requires(Forall("c" :: typeCallId,
            (s"${state_callops}_1".get("c") !== (noop $()))
              ==> (state_origin.get("c") === s"${state_origin}_1".get("c")))
          ),
          Requires(Forall("c" :: typeCallId,
            (s"${state_callops}_2".get("c") !== (noop $()))
              ==> (state_origin.get("c") === s"${state_origin}_2".get("c")))
          ),
          // invocations = union
          Requires(Forall("i" :: typeInvocationId,
            (s"${state_invocations}_1".get("i") !== (noInvocation $()))
              ==> (state_invocations.get("i") === s"${state_invocations}_1".get("i")))
          ),
          Requires(Forall("i" :: typeInvocationId,
            (s"${state_invocations}_2".get("i") !== (noInvocation $()))
              ==> (state_invocations.get("i") === s"${state_invocations}_2".get("i")))
          ),
          Requires(Forall("i" :: typeInvocationId,
            (state_invocations.get("i") !== (noInvocation $()))
              ==> ((s"${state_invocations}_1".get("i") !== (noInvocation $())) || (s"${state_invocations}_2".get("i") !== (noInvocation $())))
          )),
          // invocation results = union
          Requires(Forall("i" :: typeInvocationId,
            (s"${state_invocations}_1".get("i") !== (noInvocation $()))
              ==> (state_invocationResult.get("i") === s"${state_invocationResult}_1".get("i")))
          ),
          Requires(Forall("i" :: typeInvocationId,
            (s"${state_invocations}_2".get("i") !== (noInvocation $()))
              ==> (state_invocationResult.get("i") === s"${state_invocationResult}_2".get("i")))
          ),
          // invocation happens before = union
          Requires(Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
            (state_invocationHappensBefore.get("i1", "i2") === (s"${state_invocationHappensBefore}_1".get("i1", "i2") || s"${state_invocationHappensBefore}_2".get("i1", "i2"))))
          ),
          //


          // no happens-before relation between new calls
          // (if x happened before y and y is in s1, then x is also in s1 )
          Requires("true")
        )


          // check if invariant holds for the merged state:
          ++ invariants.map(inv => Ensures(inv))

        ,
        otherSpecs = List(),
        body = Tuple(List())
      )
    )
  }


  /**
    * adds the given postfix to all occurrences of a state-variable
    */
  def postfixStateVars(term: Term, postfix: String): Term = {

    def visitSpec(s: Spec): Spec = s match {
      case Requires(formula) => Requires(visit(formula))
      case Ensures(formula) => Ensures(visit(formula))
      case Returns(cases) => Returns(cases.map(visitFormulaCase))
      case Reads(terms) => Reads(terms.map(visit))
      case Writes(terms) => Writes(terms.map(visit))
      case RaisesName(raised) => RaisesName(raised)
      case Raises(cases) => Raises(cases.map(visitRaiseCase))
      case Variant(variants) => Variant(variants.map(visitOneVariant))
    }

    def visitVariant(s: Variant): Variant = Variant(s.variants.map(visitOneVariant))

    def visitFormulaCase(f: FormulaCase): FormulaCase =
      f.copy(formula = visit(f.formula))

    def visitRaiseCase(r: RaisesCase): RaisesCase =
      r.copy(formula = visit(r.formula))

    def visitOneVariant(v: OneVariant): OneVariant =
      v.copy(term = visit(v.term))

    def visitInv(i: Invariant): Invariant =
      Invariant(visit(i.formula))

    def visitCase(c: TermCase): TermCase =
      TermCase(c.pattern, visit(c.term))

    def visitTermField(t: TermField): TermField =
      TermField(t.fieldName, visit(t.term))

    def visit(t: Term): Term = t match {
      case IntConst(value) => t
      case RealConstant(value) => t
      case BoolConst(value) => t
      case Symbol(name) =>
        if (stateVars.exists(v => v.name.toString == name.toString)) {
          Symbol(name + postfix)
        } else {
          t
        }
      case FunctionCall(funcName, args) =>
        FunctionCall(funcName, args.map(visit))
      case ArrayLookup(arrayTerm, indexTerm) =>
        ArrayLookup(visit(arrayTerm), visit(indexTerm))
      case ArrayUpdate(arrayTerm, indexTerm, newValue) =>
        ArrayUpdate(visit(arrayTerm), visit(indexTerm), visit(newValue))
      case Conditional(condition, ifTrue, ifFalse) =>
        Conditional(visit(condition), visit(ifTrue), visit(ifFalse))
      case LambdaAbstraction(params, specs, otherSpecs, body) =>
        LambdaAbstraction(params, specs.map(visitSpec), otherSpecs.map(visitSpec), visit(body))
      case LetTerm(pattern, value, body) =>
        LetTerm(pattern, visit(value), visit(body))
      case Sequence(terms) =>
        Sequence(terms.map(visit))

      case Loop(invs, variant, body) =>
        Loop(invs.map(visitInv), variant.map(visitVariant), visit(body))
      case While(condition, invs, variant, body) =>
        While(visit(condition), invs.map(visitInv), variant.map(visitVariant), visit(body))
      case AnyTerm(typ, specs) =>
        AnyTerm(typ, specs.map(visit))
      case MatchTerm(terms, cases) =>
        MatchTerm(terms.map(visit), cases.map(visitCase))
      case QuantifierTerm(quantifier, binders, body) =>
        QuantifierTerm(quantifier, binders, visit(body))
      case Tuple(values) =>
        Tuple(values.map(visit))
      case RecordTerm(fields) =>
        RecordTerm(fields.map(visitTermField))
      case FieldAccess(recordTerm, fieldName) =>
        FieldAccess(visit(recordTerm), fieldName)
      case FieldAssignment(recordTerm, fieldName, newValue) =>
        FieldAssignment(visit(recordTerm), fieldName, visit(newValue))
      case FieldUpdate(recordTerm, fieldUpdates) =>
        FieldUpdate(visit(recordTerm), fieldUpdates.map(visitTermField))
      case CastTerm(ter, typ) =>
        CastTerm(visit(ter), typ)
      case LabeledTerm(label, ter) =>
        LabeledTerm(label, visit(ter))
      case CodeMark(name) =>
        CodeMark(name)
      case Old(ter) =>
        Old(visit(ter))
      case Assert(formula) =>
        Assert(visit(formula))
      case Assume(formula) =>
        Assume(visit(formula))
      case Check(formula) =>
        Check(visit(formula))
    }

    return visit(term)
  }


  val beginAtomic: String = "beginAtomic"

  val wellFormed: String = "wellFormed"

  def generatedIdAssumptions(): List[Ensures] = {
    for (idType <- newIdTypes) yield {
      val t = TypeSymbol(idType)

      // for every call containing a locally generated id
      // we can find a call from the current invocation containing the id and happening before
      Ensures(
        Forall(List("c" :: typeCallId, "id" :: t),
          (containsId(idType).$(state_callops.get("c"), "id")
            && state_locallyGenerated(idType).get("id"))
            ==> Exists("lc" :: typeCallId,
            containsId(idType).$(state_callops.get("lc"), "id")
              && state_happensbefore.get("lc", "c")
              && state_origin.get("lc") === "currentInvocation"
          )
        )
      )
    }
  }

  /**
    * procedure to start a transaction
    */
  def makeProcBeginAtomic(): AbstractFunction = {

    // beginAtomic can change all state-vars
    val writes: List[Symbol] = List(
      state_callops,
      state_visiblecalls,
      state_happensbefore,
      state_sametransaction,
      state_currenttransaction,
      state_origin,
      state_invocations,
      state_invocationResult,
      state_invocationHappensBefore
    )
    builtinFuncWrites += (beginAtomic -> writes)

    AbstractFunction(
      name = beginAtomic,
      params = List(currentInvocation :: typeInvocationId),
      returnType = unitType(),
      specs = List(
        Writes(writes),
        // new transaction has no calls yet:
        Ensures(Forall("c" :: typeCallId, !state_currenttransaction.get("c"))),
        // well formed history:
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => Symbol(g.name))))
      )
        // invariant maintained:
        ++ invariants.map(Ensures)
        ++ List(
        // causally consistent:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c2") && state_happensbefore.get("c1", "c2"))
              ==> state_visiblecalls.get("c1"))),
        // transaction consistent:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c1") && state_sametransaction.get("c1", "c2"))
              ==> state_visiblecalls.get("c2"))),
        // monotonic growth of visiblecalls
        Ensures(
          Forall("c" :: typeCallId, Old(state_visiblecalls).get("c")
            ==> state_visiblecalls.get("c"))),
        // monotonic growth of callops
        Ensures(
          Forall("c" :: typeCallId, (Old(state_callops).get("c") !== noop $())
            ==> (state_callops.get("c") === Old(state_callops).get("c")))),
        // monotonic growth of happensbefore
        // --> no new calls can be added before:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), (Old(state_callops).get("c2") !== noop $())
            ==> (state_happensbefore.get("c1", "c2") === Old(state_happensbefore).get("c1", "c2")))),
        // monotonic growth of sameTransaction
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), (Old(state_callops).get("c2") !== noop $())
            ==> (state_sametransaction.get("c1", "c2") === Old(state_sametransaction).get("c1", "c2")))),
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), (Old(state_callops).get("c1") !== noop $())
            ==> (state_sametransaction.get("c1", "c2") === Old(state_sametransaction).get("c1", "c2")))),
        // monotonic growth of origin
        Ensures(
          Forall("c" :: typeCallId, (Old(state_callops).get("c") !== noop $())
            ==> (state_origin.get("c") === Old(state_origin).get("c")))),
        // monotonic growth of invocations
        Ensures(
          Forall("i" :: typeInvocationId, (Old(state_invocations).get("i") !== noInvocation $())
            ==> (state_invocations.get("i") === Old(state_invocations).get("i")))),
        // monotonic growth of invocationResult
        Ensures(
          Forall("i" :: typeInvocationId, (Old(state_invocationResult).get("i") !== NoResult $())
            ==> (state_invocationResult.get("i") === Old(state_invocationResult).get("i")))),
        // monotonic growth of invocationHappensBefore
        // --> no new calls can be added before:
        Ensures(
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId), (Old(state_invocationResult).get("i2") !== NoResult $())
            ==> (state_invocationHappensBefore.get("i1", "i2") === Old(state_invocationHappensBefore).get("i1", "i2"))))
      ) ++ generatedIdAssumptions()
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

    builtinFuncWrites += (endAtomic -> List())

    AbstractFunction(
      name = endAtomic,
      params = List(),
      returnType = unitType(),
      specs =
        invariants.map(Requires)
    )
  }


  val crdtOperation: String = "crdtOperation"

  val currentInvocation = "currentInvocation"

  /**
    * a procedure to execute a CRDT operation
    */
  def makeProcCrdtOperation(): AbstractFunction = {


    val newCallId: Expr = "result"

    val writes: List[Symbol] = List(state_callops, state_happensbefore, state_visiblecalls, state_sametransaction, state_currenttransaction, state_origin)
    builtinFuncWrites += (crdtOperation -> writes)

    AbstractFunction(
      name = crdtOperation,
      params = List(currentInvocation :: typeInvocationId, operation :: typeOperation),
      returnType = typeCallId,
      specs = List(
        Writes(writes),
        Ensures(
          Old(state_callops.get(newCallId)) === (noop $())),
        Ensures(state_callops.get(newCallId) === operation),
        Ensures(Forall("c1" :: typeCallId, ("c1" !== newCallId) ==> (state_callops.get("c1") === Old(state_callops).get("c1")))),
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_happensbefore.get("c1", "c2")
              <==> (Old(state_happensbefore).get("c1", "c2")
              || ((state_visiblecalls.get("c1") || "c1" === "c2") && "c2" === newCallId)))),
        Ensures(
          Forall("c1" :: typeCallId, state_visiblecalls.get("c1")
            <==> (Old(state_visiblecalls).get("c1") || "c1" === newCallId))),
        // TODO update current transaction and sameTransaction
        // current transaction update:
        Ensures(
          Forall("c" :: typeCallId,
            state_currenttransaction.get("c") <==> (Old(state_currenttransaction).get("c") || ("c" === newCallId)))),
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            state_sametransaction.get("c1", "c2") <==> (Old(state_sametransaction).get("c1", "c2")
              || (state_currenttransaction.get("c1") && state_currenttransaction.get("c2")))
          )),
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        // update state_origin
        Ensures(
          Forall("c" :: typeCallId,
            ("c" !== newCallId) ==> (state_origin.get("c") === Old(state_origin).get("c")))),
        Ensures(
          state_origin.get(newCallId) === "currentInvocation")
      )

    )
  }


  val startInvocation: String = "startInvocation"

  val newInvocId: String = "newInvocId"

  val result: Term = "result"

  /**
    * a procedure used at the start of each invocation to setup the local state etc.
    */
  def makeStartInvocationProcedure(): AbstractFunction = {

    val writes: List[Symbol] = List(state_invocations)
    builtinFuncWrites += (startInvocation -> writes)

    val noLocallyGenerated: List[Ensures] = for (idType <- newIdTypes) yield {
      val t: TypeExpression = TypeSymbol(idType)
      Ensures(Forall("id" :: t, !state_locallyGenerated(idType).get("id")))
    }

    println(s"no locally generated: $noLocallyGenerated")


    AbstractFunction(
      name = startInvocation,
      params = List("invocation" :: typeInvocationInfo),
      returnType = typeInvocationId,
      specs = List(
        Writes(writes),
        // one fresh invocation added:
        Ensures(
          Old(state_invocations).get(result) === noInvocation.$()),
        Ensures(
          state_invocationResult.get(result) === NoResult.$()),
        Ensures(
          state_invocations.get(result) === "invocation"),
        // other invocations unchanged:
        Ensures(
          Forall("i" :: typeInvocationId, ("i" !== result) ==> (state_invocations.get("i") === Old(state_invocations).get("i"))))
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
      ) ++ noLocallyGenerated
    )
  }

  val finishInvocation: String = "finishInvocation"

  /**
    * a procedure used at the end of each invocation
    */
  def makeFinishInvocationProcedure(): AbstractFunction = {

    val writes: List[Symbol] = List(state_invocationResult, state_invocationHappensBefore)
    builtinFuncWrites += (finishInvocation -> writes)

    AbstractFunction(
      name = finishInvocation,
      params = List(newInvocId :: typeInvocationId, "res" :: typeInvocationResult),
      returnType = unitType(),
      specs = List(
        Writes(writes),
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
          Forall("i" :: typeInvocationId, ("i" !== newInvocId) ==> (state_invocationResult.get("i") === Old(state_invocationResult).get("i")))),
        // new invocation not in hb before the call (TODO move to wellformed)
        Ensures(
          Forall("i" :: typeInvocationId, !Old(state_invocationHappensBefore).get("i", newInvocId))),
        Ensures(
          Forall("i" :: typeInvocationId, !Old(state_invocationHappensBefore).get(newInvocId, "i"))),
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
              && Old(state_origin).get("c1") === "i"
              && Old(state_origin).get("c2") === newInvocId)
              ==> state_happensbefore.get("c1", "c2")
          )
        ),
        // TODO real version:
        Ensures(
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
            state_invocationHappensBefore.get("i1", "i2") === (
              // either already in old hb
              Old(state_invocationHappensBefore).get("i1", "i2")
                // or part of the new hb
                || (("i2" === newInvocId)
                && Exists("c" :: typeCallId, Old(state_origin).get("c") === newInvocId)
                && Exists("c" :: typeCallId, state_origin.get("c") === "i1")
                && Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
                ((state_origin.get("c1") === "i1") && Old(state_origin).get("c2") === newInvocId) ==> state_happensbefore.get("c1", "c2"))))))
      )
    )
  }

  /**
    * a function that takes all state vars and checks whether the state is well-formed
    */
  def makeFunc_WellFormed(): LogicDecls = {
    val i: Expr = "i"
    val body = wellformedConditions().reduce(_ && _)
    LogicDecls(List(
      LogicDecl(
        name = wellFormed,
        params = stateVars.map(g => g.name :: g.typ),
        returnType = TypeBool(),
        implementation = Some(body)
      )
    ))
  }

  /**
    * the conditions required to check well-formedness
    */
  def wellformedConditions(): List[Term] = {
    val i: Expr = "i"
    List(
      // no happensBefore relation between non-existing calls
      "happensBefore_exists_l" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_callops.get("c1") === (noop $()))
            ==> !state_happensbefore.get("c1", "c2")
        ),
      "happensBefore_exists_r" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_callops.get("c2") === (noop $()))
            ==> !state_happensbefore.get("c1", "c2")
        ),
      // visible calls are a subset of all calls
      "visibleCalls_exist" %%:
        Forall("c" :: typeCallId, state_visiblecalls.get("c") ==> (state_callops.get("c") !== (noop $()))),
      // visible calls forms consistent snapshot
      "visibleCalls_transaction_consistent1" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_visiblecalls.get("c2") && state_sametransaction.get("c1", "c2")) ==> state_visiblecalls.get("c1")),
      "visibleCalls_transaction_consistent2" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_visiblecalls.get("c2") && state_sametransaction.get("c2", "c1")) ==> state_visiblecalls.get("c1")),
      "visibleCalls_causally_consistent" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_visiblecalls.get("c2") && state_happensbefore.get("c1", "c2")) ==> state_visiblecalls.get("c1")),

      // happensBefore is a partial order (reflexivity, transitivity, antisymmetric)
      "happensBefore_reflex" %%:
        Forall("c" :: typeCallId, (state_callops.get("c") !== (noop $())) ==> state_happensbefore.get("c", "c")),
      "happensBefore_trans" %%:
        Forall(List("x" :: typeCallId, "y" :: typeCallId, "z" :: typeCallId),
          (state_happensbefore.get("x", "y") && state_happensbefore.get("y", "z")) ==> state_happensbefore.get("x", "z")),
      "happensBefore_antisym" %%:
        Forall(List("x" :: typeCallId, "y" :: typeCallId), (state_happensbefore.get("x", "y") && state_happensbefore.get("y", "x")) ==> ("x" === "y")),
      // invocation happens-before of origins implies happens-before of calls
      "happensBefore_reflex" %%:
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
        state_invocationHappensBefore.get("i1", "i2") ==> (state_invocationResult.get("i2") !== NoResult.$())),
      // no sameTransaction relation between non-existing calls
      Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
        (state_callops.get("c1") === (noop $()))
          ==> !state_sametransaction.get("c1", "c2")
      ),
      Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
        (state_callops.get("c2") === (noop $()))
          ==> !state_sametransaction.get("c1", "c2")
      ),
      // sameTransaction is a equivalence relation (reflexive, transitive, symmetric)
      Forall("c" :: typeCallId, (state_callops.get("c") !== (noop $())) ==> state_sametransaction.get("c", "c")),
      Forall(List("x" :: typeCallId, "y" :: typeCallId, "z" :: typeCallId),
        (state_sametransaction.get("x", "y") && state_sametransaction.get("y", "z")) ==> state_sametransaction.get("x", "z")),
      Forall(List("x" :: typeCallId, "y" :: typeCallId), state_sametransaction.get("x", "y") === state_sametransaction.get("y", "x")),
      // transaction consistency with happens before:
      Forall(List("x1" :: typeCallId, "x2" :: typeCallId, "y1" :: typeCallId, "y2" :: typeCallId),
        (state_sametransaction.get("x1", "x2")
          && state_sametransaction.get("y1", "y2")
          && !state_sametransaction.get("x1", "y1")
          && state_happensbefore.get("x1", "y1"))
          ==> state_happensbefore.get("x2", "y2"))

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
    AnyTerm(transformTypeExpr(typ))
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


  def assignedVars(term: Term): Set[String] = {
    var result = Set[String]()
    walk(term) {
      case FunctionCall(LQualid(List(), LIdent(":=")), List(Symbol(left), _right)) =>
        result += left.toString
      case FunctionCall(LQualid(List(), LIdent(m)), _args) if builtinFuncWrites.contains(m) =>
        result ++= builtinFuncWrites(m).map(_.name.toString)
    }
    result
  }

  /**
    * Transforms a procedure into a why-function with the
    * pre- and post-conditions that need to be checked
    */
  def transformProcedure(procedure: InProcedure): GlobalLet = {


    val procname: String = procedure.name.name
    val params: List[TypedParam] = procedure.params.map(transformVariable)
    val paramNames: List[Symbol] = params.map(p => IdentifierExpr(p.name))
    val specContext: Context = Context(
      procedureName = procname,
      procedureArgNames = paramNames,
      refVars = procedure.locals.map(_.name.name).toSet
    )

    val bodyCtxt = specContext.copy(targetIsLogic = false)


    val body = LetTerm(
      pattern = VariablePattern("new" + InvocationId),
      value = FunctionCall(startInvocation, List(FunctionCall(invocationInfoForProc(procname), paramNames))),
      body = makeBlock(
        // call endAtomic to check invariants (TODO make extra procedure to check invariants)
        FunctionCall(endAtomic, List()),
        // execute procedure body:
        transformStatement(procedure.body)(bodyCtxt),
        if (procedure.returnType.isEmpty) {
          makeReturn(None, List(), procedure)(bodyCtxt)
        } else {
          makeBlock()
        },
        Tuple(List())
      )
    )

    val bodyWithLocals = transformLocals(procedure.locals)(body)

    val writes = assignedVars(bodyWithLocals)
      .filter(w => stateVars.exists(v => v.name.name == w))
      .map(Symbol(_))
      .toList

    GlobalLet(
      name = procname,
      funBody = FunBody(
        params = params,
        returnType = Some(unitType()),
        specs = List()
          ++ wellformedConditions().map(Requires)
          ++ invariants.map(Requires)
          ++ List(
          Writes(writes),
          Reads(stateVars.map(g => IdentifierExpr(g.name))))
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
    FunctionCall(beginAtomic, List(newInvocationId)),
    captureState(context, "begin atomic"),
    transformStatement(context.body)(ctxt.copy(isInAtomic = true)),
    captureState(context, "before commit"),
    FunctionCall(endAtomic, List(unit())).setTrace(EndAtomicTraceInfo(context)),
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


  val newInvocationId: Symbol = "new" + InvocationId

  def transformCrdtCall(context: CrdtCall)(implicit ctxt: Context): Term = {
    makeBlock(
      FunctionCall(crdtOperation, List(newInvocationId, transformFunctioncall(context.call))),
      unit())
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
      Assert(transformExpr(expr)(ctxt.copy(targetIsLogic = true)))
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
        var va: Term = IdentifierExpr(name)
        if (ctxt.isRefVar(name)) {
          va = va.deref()
        }
        va
      case InputAst.BoolConst(_, _, boolVal) =>
        BoolConst(boolVal)
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
          state_invocationHappensBefore.get(args.head, args(1))
        } else {
          state_happensbefore.get(args.head, args(1))
        }
      case BF_sameTransaction() =>
        state_sametransaction.get(args.head, args(1))
      case BF_isVisible() =>
        state_visiblecalls.get(args.head)
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
        state_callops.get(args.head)
      case BF_getInfo() =>
        state_invocations.get(args.head)
      case BF_getResult() =>
        state_invocationResult.get(args.head)
      case BF_getOrigin() =>
        state_origin.get(args.head)
      case BF_inCurrentInvoc() =>
        state_origin.get(args.head) === "new" + InvocationId
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
    val typ = transformTypeExpr(context.typename)
    makeBlock(
      // nondeterministic creation of new id
      Havoc(varName, typ),
      // we can assume that the new id was never used in an operation before
      newIdAssumptions(typ, varName) // TODO move into any-specs
    )

  }

  def Havoc(varName: String, typ: TypeExpression) = Assignment(varName, AnyTerm(typ))

  def newIdAssumptions(typeName: TypeExpression, idName: String): Term = {
    // add axioms for contained ids
    var result = List[Term]()

    val generated = state_locallyGenerated(typeName.stringName)
    result ++= List(
      Assignment(generated, "Map.set".$(generated.deref(), IdentifierExpr(idName).deref(), BoolConst(true)))
    )

    for ((opName, args2) <- operationDefs) {
      val args = args2.map(v => v.copy(name = "_p_" + v.name))
      val idType = typeName
      val argIds: List[Symbol] = args.map(a => IdentifierExpr(a.name))
      result = result ++ (for (arg <- args; if arg.typ == idType) yield {
        Assume(Forall(("c" :: typeCallId) +: args,
          (state_callops.get("c") === FunctionCall(operationCaseName(opName), argIds))
            ==> (IdentifierExpr(idName).deref() !== IdentifierExpr(arg.name))))
      })
    }


    makeBlockL(result)
  }

  def transformReturnStmt(context: InputAst.ReturnStmt)(implicit ctxt: Context): Term = {
    val returnedExpr: Expr = transformExpr(context.expr)
    makeReturn(Some(returnedExpr), context.assertions, context)
  }


  def makeReturn(returnedExpr: Option[Expr], endAssertions: List[AssertStmt], source: InputAst.AstElem)(implicit ctxt: Context): Term = {
    val procRes = FunctionCall(invocationResForProc(ctxt.procedureName), returnedExpr.toList)

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

    funcName = functionReplacements.getOrElse(funcName, funcName)

    if (queryFunctions.contains(funcName)) {
      // add state vars for query-functions
      args ++= stateVars.map(g => IdentifierExpr(g.name))

      if (!ctxt.targetIsLogic) {
        funcName = s"${funcName}_proc"
      }

    } else if (procedureNames.contains(funcName)) {
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
