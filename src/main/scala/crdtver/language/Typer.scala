package crdtver.language

import crdtver.Repliss
import crdtver.Repliss._
import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst._
import crdtver.language.TypedAst.{AnyType, BoolType, BuiltinDefinition, CallIdType, CrdtTypeDefinitionType, DatabaseCall, Definition, DependentReturnType, FunctionKind, IdType, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, NoDefinition, OperationType, SimpleType, SomeOperationType, TransactionIdType, TypeUnit}
import crdtver.language.TypedAst.FunctionKind.FunctionKindDatatypeConstructor
import crdtver.language.crdts.CrdtTypeDefinition.Operation
import crdtver.language.crdts.{CrdtInstance, CrdtTypeDefinition, NameContext, UniqueName}
import crdtver.language.{TypedAst => typed}
import crdtver.utils.EitherExtensions.EitherUtils
import crdtver.utils.{Err, Ok}
import info.debatty.java.stringsimilarity.JaroWinkler

/**
  * Code for typing an InputProgram.
  *
  * checkProgram returns an AST annotated with types.
  */
class Typer(nameContext: NameContext) {

  // names for builtins:
  private val noResult = nameContext.newName("NoResult")
  private val newInvocationId = nameContext.newName("newInvocationId")


  private var errors: List[Error] = List[Repliss.Error]()

  trait TypeContext {
    def declaredTypes: Map[String, typed.InTypeExpr]

    def crdtContext: NameContext
  }

  case class TypeContextImpl(declaredTypes: Map[String, typed.InTypeExpr], crdtContext: NameContext) extends TypeContext


  case class Context(
    // types of variables
    types: Map[String, Definition], // TODO rename to definitions
    declaredTypes: Map[String, typed.InTypeExpr],
    datatypes: Map[String, Map[String, typed.FunctionType]],
    typeDecls: Map[UniqueName, typed.InTypeDecl],
    // expected return value of current procedure
    expectedReturn: Option[typed.InTypeExpr] = None,
    toplevelCrdtOperations: List[Operation] = List(),
    //
    crdtContext: NameContext
  ) extends TypeContext {
    def withBinding(varname: String, definition: Definition): Context = {
      copy(
        types = types + (varname -> definition)
      )
    }
  }

  private def splitEitherList[A, B](el: List[Either[A, B]]): (List[A], List[B]) = {
    val (lefts, rights) = el.partition(_.isLeft)
    (lefts.map(_.left.get), rights.map(_.right.get))
  }

  private def toInstance(scope: String, c: InCrdtType)(implicit ctxt: Context): Either[CrdtInstance, typed.InTypeExpr] = {
    c match {
      case InCrdt(source, name, typ) =>
        val list: List[Either[CrdtInstance, typed.InTypeExpr]] = typ.map(toInstance(scope, _))
        val (crdtArgs, typeArgs) = splitEitherList(list)
        for (crdt <- CrdtTypeDefinition.crdts) {
          if (crdt.name == name.name) {
            crdt.makeInstance(scope, typeArgs, crdtArgs, ctxt.crdtContext) match {
              case Ok(instance) =>
                return Left(instance)
              case Err(error) =>
                addError(c, error)
            }
          }
        }

        Right(checkType(InputAst.UnresolvedType(name.name)(name.source)))
      case InStructCrdt(source, keyDecl) =>
        var fields = Map[UniqueName, CrdtInstance]()
        for (key <- keyDecl.iterator) {
          toInstance(scope + "_" + key.name.name, key.crdttype) match {
            case Left(a) =>
              val name = ctxt.crdtContext.newName(key.name.name)
              fields += (name -> a)
            case Right(b) => println("Invalid arguments given")
          }
        }
        return Left(StructInstance(scope,  fields, ctxt.crdtContext))
    }
  }

  //  private def crdtunfold(nameBindings: Map[String, typed.InTypeExpr], key: InKeyDecl)(implicit ctxt: Context): Map[String, typed.InTypeExpr] = {
  //    var tempBindings = nameBindings
  //    toInstance(key.crdttype) match {
  //      case Left(a) =>
  //        for (op <- a.operations; if op.isQuery) {
  //          val opName = key.name.name + '_' + op.name
  //          if (tempBindings.contains(opName)) {
  //            addError(key.crdttype, s"Element with name $opName already exists.")
  //          }
  //          val paramTypes = op.paramTypes
  //          tempBindings += (opName -> typed.FunctionType(paramTypes, typed.OperationType(opName)(), FunctionKindDatatypeConstructor())())
  //        }
  //      case Right(AnyType()) =>
  //      // avoid cascading errors
  //      case Right(b) =>
  //        addError(key.crdttype, "Invalid type: " + b)
  //    }
  //    return tempBindings
  //  }

//  private def crdtOperations(key: InKeyDecl)(implicit ctxt: Context): List[Operation] = {
//    toInstance(key.crdttype) match {
//      case Left(a) =>
//        a.operations
//      case Right(AnyType()) =>
//        // avoid cascading errors
//        List()
//      case Right(b) =>
//        addError(key.crdttype, "Invalid type: " + b)
//        List()
//    }
//  }

  private def addError(elem: AstElem, msg: String): Unit = {
    val source = elem.getErrorSource
    addError(source, msg)
  }

  private def addError(source: SourceTrace, msg: String): Unit = {
    val err = Error(source.range, msg)
    errors :+= err
  }

  private def addError(elem: typed.AstElem, msg: String): Unit = {
    val source = elem.getErrorSource
    addError(source, msg)
  }

  def checkProgram(program: InProgram): Result[typed.InProgram] = {
    var nameBindings = Map[String, Definition](
      noResult.originalName -> typed.BuiltinDefinition(noResult, typed.functionType(List(), typed.InvocationResultType(), typed.FunctionKind.FunctionKindDatatypeConstructor())())
    )
    var declaredTypes = Map[String, typed.InTypeExpr](
      "boolean" -> typed.BoolType(),
      "int" -> typed.IntType(),
      "callId" -> typed.CallIdType(),
      "invocationId" -> typed.InvocationIdType(),
      "transactionId" -> typed.TransactionIdType(),
      "invocationInfo" -> typed.InvocationInfoType()
    )
    // collect program specific types
    for (operation <- program.operations) {
      val name = operation.name.name
      if (nameBindings.contains(name) || declaredTypes.contains(name)) {
        addError(operation, s"Element with name $name already exists.")
      }
      declaredTypes += (name -> OperationType(nameContext.newName(name), TypeUnit())())
    }

    val typeName: Map[InTypeDecl, UniqueName] = (for (t <- program.types) yield t -> nameContext.newName(t.name.name)).to(Map)


    for (t <- program.types) {
      val name = t.name.name
      val uname = typeName(t)
      if (declaredTypes.contains(name)) {
        addError(t, s"Element with name $name already exists.")
      }

      if (t.isIdType) {
        declaredTypes += (name -> IdType(uname)())
        if (t.dataTypeCases.nonEmpty) {
          addError(t, s"Id type $name cannot be a datatype.")
        }
      } else {
        declaredTypes += (name -> SimpleType(uname)())
      }
    }

    for (crdt <- CrdtTypeDefinition.crdts) {
      declaredTypes += (crdt.name -> CrdtTypeDefinitionType(crdt))
    }


    var datatypes = Map[UniqueName, Map[UniqueName, typed.FunctionType]]()
    var typeDecls = Map[UniqueName, typed.InTypeDecl]()


    val typeContext = TypeContextImpl(declaredTypes, nameContext)


    // build toplevel context:
    // TODO add custom queries and operations
    //    for (query <- program.queries) {
    //      val name = query.name.name
    //      if (nameBindings contains name) {
    //        addError(query, s"Element with name $name already exists.")
    //      }
    //      nameBindings += (name -> typed.functionType(query.params.map(t => checkType(t.typ)(typeContext)), checkType(query.returnType)(typeContext), FunctionKindCrdtQuery())())
    //    }
    //
    //    for (operation <- program.operations) {
    //      val name = operation.name.name
    //      nameBindings += (name -> typed.functionType(operation.params.map(t => checkType(t.typ)(typeContext)), OperationType(nameContext.newName(name), TypeUnit())(), FunctionKindDatatypeConstructor())())
    //    }


    for (t <- program.types) {
      val tt = declaredTypes.getOrElse(t.name.name,
        throw new RuntimeException(s"Could not find type ${t.name}"))
      val t_uname = typeName(t)
      val dtCases: List[(UniqueName, Definition)] =
        for (c <- t.dataTypeCases) yield {
          val c_name = c.name.name
          val c_uname = nameContext.newName(c_name)
          val typedParams = checkParams(c.params)(typeContext)

          val ct = TypedAst.DataTypeCase(
            c.getSource,
            c_uname,
            typedParams,
            tt
          )
          c_uname -> ct
        }
      if (dtCases.nonEmpty) {
        nameBindings = nameBindings ++ dtCases.map { case (k, e) => k.originalName -> e }
        val dtCasesMap: Map[UniqueName, typed.FunctionType] =
          dtCases.map { case (k, e) => (k, e.typ.asInstanceOf[typed.FunctionType]) }.toMap
        datatypes += t_uname -> dtCasesMap
      }
    }


    //    for (crdt <- program.crdts) {
    //      implicit val typeCtxt: Context = Context(
    //        types = nameBindings,
    //        declaredTypes = declaredTypes,
    //        datatypes = datatypes,
    //        crdtContext = crdtContext
    //      )
    //      nameBindings = nameBindings ++ crdtunfold(nameBindings, crdt.keyDecl)
    //    }

    val crdts: Map[String, (UniqueName, CrdtInstance)] = {
      implicit val typeCtxt: Context = Context(
                types = nameBindings,
                declaredTypes = declaredTypes,
                datatypes = datatypesToNormalNames(datatypes),
                typeDecls = typeDecls,
                crdtContext = nameContext
              )
      (for {
          crdt <- program.crdts
          instance <- toInstance(crdt.keyDecl.name.name, crdt.keyDecl.crdttype).takeLeft
        } yield {
        val name = crdt.keyDecl.name.name
        val uname = nameContext.newName(name)
          name -> (uname, instance)
        }).to(Map)
    }

    val crdtBindings: List[(String, Definition)] =
      for (crdt <- program.crdts) yield {
        implicit val typeCtxt: Context = Context(
          types = nameBindings,
          declaredTypes = declaredTypes,
          datatypes = datatypesToNormalNames(datatypes),
          typeDecls = typeDecls,
          crdtContext = nameContext
        )
        val name = crdt.keyDecl.name.name
        val (uname, instance) = crdts(name)
        val cdef = TypedAst.CrdtDefinition(uname, instance)
        name -> cdef
      }
    nameBindings = nameBindings ++ crdtBindings

    for (p <- program.procedures) {
      val paramTypes: List[InTypeExpr] = p.params.map(_.typ)
      // invocation info constructor
      val invocationInfoType = typed.functionType(paramTypes.map(checkType(_)(typeContext)), InvocationInfoType(), FunctionKindDatatypeConstructor())()
      val uname = nameContext.newName(p.name.name)
      nameBindings += (p.name.name -> BuiltinDefinition(uname, invocationInfoType))
      // invocation result constructor
      val invocationResType = typed.functionType(p.returnType.map(checkType(_)(typeContext)).toList, InvocationResultType(), FunctionKindDatatypeConstructor())()
      val invocationResTypeUname = nameContext.newName(s"${p.name.name}_res")
      nameBindings += (invocationResTypeUname.originalName -> BuiltinDefinition(invocationResTypeUname, invocationResType))
    }

    val baseContext = Context(
      types = nameBindings,
      declaredTypes = declaredTypes,
      datatypes = datatypesToNormalNames(datatypes),
      typeDecls = typeDecls, // TODO add type decls
      crdtContext = nameContext
    )




    val programCrdt = StructInstance("", fields = crdts.values.toMap, nameContext)


    {
      implicit val baseContext2: Context = baseContext.copy(
        toplevelCrdtOperations = programCrdt.operations,
      )

      val checkedProgram = typed.InProgram(
        name = program.name,
        source = program.source,
        procedures = program.procedures.map(checkProcedure),
        types = program.types.map(checkTypeDecl(_, typeName)),
        axioms = program.axioms.map(checkAxiom),
        invariants = program.invariants.map(checkInvariant),
        programCrdt = programCrdt
      )

      println(s"Checked program:\n${checkedProgram.printAst}")

      if (errors.isEmpty) {
        NormalResult(checkedProgram)
      } else {
        ErrorResult(errors)
      }
    }
  }


  private def datatypesToNormalNames(datatypes: Map[UniqueName, Map[UniqueName, TypedAst.FunctionType]]): Map[String, Map[String, TypedAst.FunctionType]] = {
    for ((n1, m) <- datatypes) yield
      n1.originalName -> (for ((n2, c) <- m) yield n2.originalName -> c)
  }

  private def checkProcedure(p: InProcedure)(implicit ctxt: Context): typed.InProcedure = {
    val typedParams = checkParams(p.params)
    val typedLocals = checkParams(p.locals)

    val vars: List[typed.InVariable] = typedParams ++ typedLocals
    val typesWithParams: Map[String, Definition] = ctxt.types ++ getArgTypes(vars)
    val newCtxt = ctxt.copy(
    types = typesWithParams,
    expectedReturn = p.returnType.map(checkType)
    )

    typed.InProcedure(
      name = ctxt.crdtContext.newName(p.name.name),
      source = p.source,
      body = checkStatement(p.body)(newCtxt),
      params = typedParams,
      locals = typedLocals,
      returnType = p.returnType.map(checkType)
    )
  }

  private def getArgTypesT(vars: List[typed.InVariable]): List[(String, typed.Definition)] = {
    for (param <- vars) yield param.name.originalName -> param
  }

  private def getArgTypes(vars: List[typed.InVariable])(implicit ctxt: Context): List[(String, Definition)] = {
    vars.map(p => p.name.originalName -> p)
  }

  private def checkTypeDecl(t: InTypeDecl, typeName: Map[InTypeDecl, UniqueName])(implicit ctxt: Context): typed.InTypeDecl = {
    // TODO checks necessary?
    val uname = typeName(t)
    typed.InTypeDecl(
      name = uname,
      source = t.source,
      isIdType = t.isIdType,
      dataTypeCases = t.dataTypeCases.map(c => typed.DataTypeCase(
        name = ctxt.crdtContext.newName(c.name.name),
        source = c.source,
        params = checkParams(c.params),
        returnTyp = SimpleType(uname)()
      ))
    )
  }

  private def checkVariable(variable: InVariable)(implicit ctxt: TypeContext): typed.InVariable =
    typed.InVariable(name = ctxt.crdtContext.newName(variable.name.name), source = variable.source, typ = checkType(variable.typ))

  private def checkParams(params: List[InVariable])(implicit ctxt: TypeContext): List[typed.InVariable] = {
    params.map(checkVariable)
  }

  def checkType(t: InTypeExpr)(implicit ctxt: TypeContext): typed.InTypeExpr = t match {
    case UnresolvedType(name) =>
      ctxt.declaredTypes.getOrElse(name, {
        val suggestions = ctxt.declaredTypes.keys.toList
        addError(t, s"Could not find type $name.\nSimilar existing types: ${suggestNamesStr(name, suggestions)}")
        typed.AnyType()
      })
    case f: FunctionType =>
      checkFunctionType(f, ???)
  }

  private def suggestNamesStr(name: String, suggestions: Iterable[String]): String = {
    suggestNames(name, suggestions).mkString(", ")
  }

  private def suggestNames(name: String, suggestions: Iterable[String]): List[String] = {
    val jw = new JaroWinkler
    val bestSuggestions = suggestions.toList.sortBy(jw.distance(_, name)).take(5)
    bestSuggestions
  }

  private def checkFunctionType(f: FunctionType, kind: FunctionKind)(implicit ctxt: TypeContext): typed.FunctionType = {
    typed.functionType(f.argTypes.map(checkType), checkType(f.returnType), kind)(source = f.getSource)
  }

  //  private def checkOperation(o: InOperationDecl)(implicit ctxt: Context): typed.InOperationDecl = {
  //    typed.InOperationDecl()
  //    o.copy(params = checkParams(o.params))
  //  }


  private def checkAxiom(p: InAxiomDecl)(implicit ctxt: Context): typed.InAxiomDecl =
    typed.InAxiomDecl(
      source = p.source,
      expr = checkExpr(p.expr)
    )

  private def checkInvariant(i: InInvariantDecl)(implicit ctxt: Context): typed.InInvariantDecl =
    typed.InInvariantDecl(
      source = i.source,
      expr = checkExpr(i.expr)
    )


  private def lookup(varname: Identifier)(implicit ctxt: Context): typed.Definition =
    lookup(varname.name, varname.source)

  private def lookup(varname: String, source: SourceTrace)(implicit ctxt: Context): typed.Definition = {
    ctxt.types.getOrElse[typed.Definition](varname, {
      addError(source, s"Could not find declaration of $varname.\nSimilar definitions: ${suggestNamesStr(varname, ctxt.types.keys)}")
      NoDefinition(UniqueName(varname, 0))
    })
  }

  private def lookupVar(varname: Identifier)(implicit ctxt: Context): TypedAst.Definition =
    lookupVar(varname.name, varname.source)

  private def lookupVar(varname: String, source: SourceTrace)(implicit ctxt: Context): TypedAst.Definition = {
    ctxt.types.getOrElse(varname, {
      addError(source, s"Could not find declaration of $varname.\nSimilar definitions: ${suggestNamesStr(varname, ctxt.types.keys)}")
      NoDefinition(UniqueName(varname, 0))
    })
  }

  private def checkStatement(s: InStatement)(implicit ctxt: Context): typed.InStatement = s match {
    case BlockStmt(source, stmts) =>
      typed.BlockStmt(source, stmts.map(checkStatement))
    case Atomic(source, body) =>
      typed.Atomic(source, checkStatement(body))
    case LocalVar(source, variable) =>
      typed.LocalVar(source, checkVariable(variable))
    case IfStmt(source, cond, thenStmt, elseStmt) =>
      val condTyped: typed.InExpr = checkExpr(cond)
      if (!condTyped.getTyp.isSubtypeOf(BoolType())) {
        addError(cond, s"Expression of if-statement must be boolean, but was ${condTyped.getTyp}.")
      }
      typed.IfStmt(source, condTyped, checkStatement(thenStmt), checkStatement(elseStmt))
    case MatchStmt(source, expr, cases) =>
      val exprTyped = checkExpr(expr)

      val casesTyped = cases.map(checkCase(_, exprTyped.getTyp))

      typed.MatchStmt(source, exprTyped, casesTyped)
    case CrdtCall(source, call) =>
      val callTyped = checkFunctionCall(call, SomeOperationType())
      callTyped match {
        case DatabaseCall(source, typ, operation) =>
          typed.CrdtCall(source, None, operation)
        case _ =>
          addError(call, s"Not an operation:\n${callTyped} : ${callTyped.getClass}\n$call : ${call.getClass}")
          typed.makeBlock(source, List())
      }
    case Assignment(source, varname, expr) =>
      val varDef = lookupVar(varname)
      val varType: typed.InTypeExpr = varDef.typ
      val exprTyped = checkExpr(expr)
      if (!exprTyped.getTyp.isSubtypeOf(varType)) {
        addError(expr, s"Expression of type ${exprTyped.getTyp} is not assignable to variable of type $varType.")
      }
      typed.Assignment(source, varDef.name, exprTyped)
    case NewIdStmt(source, varname, typename) =>
      val definition = lookupVar(varname)
      val varType: typed.InTypeExpr = definition.typ
      val t = checkType(typename)

      if (!t.isInstanceOf[IdType]) {
        addError(typename, s"Type $t must be declared as idType.")
      }
      if (!t.isSubtypeOf(varType)) {
        addError(typename, s"Cannot assign id $t to variable of type $varType.")
      }
      typed.NewIdStmt(source, definition.name, t)
    case ReturnStmt(source, expr, assertions) =>
      val typedExpr = checkExpr(expr)
      ctxt.expectedReturn match {
        case Some(rt) =>
          if (!typedExpr.getTyp.isSubtypeOf(rt)) {
            addError(typedExpr, s"Expected return value of type $rt, but found type ${typedExpr.getTyp}.")
          }
        case None =>
          addError(typedExpr, s"Cannot return value from method without return type.")
      }

      val assertionCtxt = ctxt.withBinding(newInvocationId.originalName, BuiltinDefinition(newInvocationId, InvocationIdType()))
      typed.ReturnStmt(source, typedExpr, assertions.map(checkAssertStatement(_)(assertionCtxt)))
    case s: AssertStmt =>
      checkAssertStatement(s)
  }

  private def checkAssertStatement(s: AssertStmt)(implicit ctxt: Context): typed.AssertStmt = {
    typed.AssertStmt(source = s.source, expr = checkExpr(s.expr))
  }


  private def checkCase(c: MatchCase, expectedType: typed.InTypeExpr)(implicit ctxt: Context): typed.MatchCase = {
    val (newCtxt, patternTyped) = checkPattern(c.pattern, expectedType)(ctxt)
    val stmtTyped = checkStatement(c.statement)(newCtxt)
    typed.MatchCase(
      source = c.source,
      pattern = patternTyped,
      statement = stmtTyped
    )
  }

  private def checkPattern(pattern: InPattern, expectedType: typed.InTypeExpr)(implicit ctxt: Context): (Context, typed.InPattern) = pattern match {
    case InPatternVar(source, name) =>
      if (ctxt.types.contains(name)) {
        addError(pattern, s"Variable with name $name is already defined.")
      }
      val uname = ctxt.crdtContext.newName(name)
      val tp = TypedAst.InPatternVar(source, uname, expectedType)
      val newCtxt = ctxt.withBinding(name, tp)
      (newCtxt, tp)
    case f@InPatternApply(source, functionName, args) =>
      var newCtxt = ctxt
      expectedType match {
        case SimpleType(dtName) =>
          ctxt.typeDecls.get(dtName) match {
            case None =>
              addError(pattern, s"Type $expectedType is not a datatype.")
            case Some(typeDecl) =>
              typeDecl.findDatatypeCase(functionName.name) match {
                case None =>
                  addError(pattern, s"$functionName is not a case of type $expectedType")
                case Some(dataTypeCase) =>
                  val functionType: TypedAst.FunctionType = dataTypeCase.typ

                  if (functionType.argTypes.size != args.size) {
                    addError(pattern, s"Expected (${functionType.argTypes.mkString(",")}) arguments, but got (${args.mkString(",")}")
                  }
                  val typedArgs = for ((a, t) <- args.zip(functionType.argTypes)) yield {
                    val (c, argTyped) = checkPattern(a, t)(newCtxt)
                    newCtxt = c
                    argTyped
                  }
                  return (newCtxt, typed.InPatternApply(
                    source,
                    dataTypeCase.name,
                    typedArgs
                  ))
              }

          }
        case _ =>
          addError(f, s"Cannot use functionCall with expected Type $expectedType.")
          ???

      }
      println(s"check $pattern with type ${expectedType.getClass}")
      ???
  }

  private def checkExpr(e: InExpr, expectedType: typed.InTypeExpr = typed.AnyType())(implicit ctxt: Context): typed.InExpr = e match {
    case v@VarUse(source, name) =>
      val definition = lookup(name, source)
      typed.VarUse(source, definition.typ, definition.name)
    case BoolConst(source, v) =>
      typed.BoolConst(source, BoolType(), v)
    case IntConst(source, value) =>
      typed.IntConst(source, IntType(), value)
    case fc: FunctionCall =>
      checkFunctionCall(fc, expectedType)
    case MemberCall(source, receiver, functionName2, args2) =>
      // a member call f1(args1).f2(args2)
      // is just syntactic sugar for f1(args1..., f2(args2))
      val rewritten = receiver match {
        case FunctionCall(source1, functionName1, args1) =>
          FunctionCall(source1 union source, functionName1, args1 :+
            FunctionCall(source, functionName2, args2))
        case other =>
          addError(e, "The dot notation can only be used on function calls.")
          other
      }
      checkExpr(rewritten)
    case ab@ApplyBuiltin(source, function, args) =>
      var typedArgs = args.map(checkExpr(_))
      var newFunction = function
      val tuple: (List[typed.InTypeExpr], typed.InTypeExpr) = function match {
        case BF_isVisible() =>
          List(CallIdType()) -> BoolType()
        case BF_happensBefore(_) =>
          typedArgs.headOption match {
            case Some(expr) if expr.getTyp.isSubtypeOf(CallIdType()) =>
              newFunction = BF_happensBefore(HappensBeforeOn.Call())
              List(CallIdType(), CallIdType()) -> BoolType()
            case _ =>
              newFunction = BF_happensBefore(HappensBeforeOn.Invoc())
              List(InvocationIdType(), InvocationIdType()) -> BoolType()
          }
        case BF_sameTransaction() =>
          List(CallIdType(), CallIdType()) -> BoolType()
        case BF_less() | BF_lessEq() | BF_greater() | BF_greaterEq() =>
          List(IntType(), IntType()) -> BoolType()
        case BF_equals() | BF_notEquals() =>
          val a1 = checkExpr(args(0))
          val t1 = a1.getTyp
          val a2 = checkExpr(args(1), t1)
          val t2 = a2.getTyp
          typedArgs = List(a1, a2)
          if (!t1.isSubtypeOf(t2) && !t2.isSubtypeOf(t1)) {
            addError(e, s"Cannot compare $t1 with $t2")
          }
          List(AnyType(), AnyType()) -> BoolType()
        case BF_and() | BF_or() | BF_implies() =>
          List(BoolType(), BoolType()) -> BoolType()
        case BF_plus() | BF_minus() | BF_mult() | BF_div() | BF_mod() =>
          List(IntType(), IntType()) -> IntType()
        case BF_not() =>
          List(BoolType()) -> BoolType()
        case BF_getOperation() =>
          List(CallIdType()) -> SomeOperationType()
        case BF_getInfo() =>
          List(InvocationIdType()) -> InvocationInfoType()
        case BF_getResult() =>
          List(InvocationIdType()) -> InvocationResultType()
        case BF_getOrigin() =>
          List(CallIdType()) -> InvocationIdType()
        case BF_getTransaction() =>
          List(CallIdType()) -> TransactionIdType()
        case BF_inCurrentInvoc() =>
          List(CallIdType()) -> BoolType()
      }
      val (argTypes, t) = tuple
      checkCall(ab.source, typedArgs, argTypes)
      typed.ApplyBuiltin(
        source = ab.source,
        typ = t,
        function = newFunction,
        args = typedArgs
      )
    case qe@QuantifierExpr(source, quantifier, vars, expr) =>
      val typedVars = checkParams(vars)
      val newCtxt = ctxt.copy(
        types = ctxt.types ++ getArgTypesT(typedVars)
      )
      val exprTyped = checkExpr(expr)(newCtxt)
      if (!exprTyped.getTyp.isSubtypeOf(BoolType())) {
        addError(expr, s"Expression inside quantifier expression must be boolean, but type was ${exprTyped.getTyp}.")
      }
      typed.QuantifierExpr(
        source = source,
        quantifier = quantifier,
        typ = BoolType(),
        vars = typedVars,
        expr = exprTyped)
  }


  private def checkCall(source: SourceTrace, typedArgs: List[typed.InExpr], argTypes: List[typed.InTypeExpr]): Unit = {
    if (argTypes.length != typedArgs.length) {
      addError(source, s"Expected (${argTypes.mkString(", ")}) arguments, but (${typedArgs.map(a => a.getTyp).mkString(", ")}) arguments were given.")
    } else {
      for ((et, arg) <- argTypes.zip(typedArgs)) {
        if (!arg.getTyp.isSubtypeOf(et)) {
          addError(arg, s"Expected argument of type $et, but got ${arg.getTyp}.")
        }
      }
    }
  }

  private def resolveDependentReturn(returnType: TypedAst.InTypeExpr, argTypes: List[TypedAst.InTypeExpr]): TypedAst.InTypeExpr = {
    returnType match {
      case DependentReturnType(operations) =>
        argTypes.lastOption match {
          case Some(OperationType(name, resultType)) =>
            return resultType
          case _ =>
        }
      case _ =>
    }
    returnType
  }

  private def checkFunctionCall(fc: FunctionCall, expectedType: typed.InTypeExpr)(implicit ctxt: Context): typed.CallExpr = {
    expectedType match {
      case SomeOperationType() =>
        val t = SomeOperationType()
        val op = checkFunctionCallForExpectedOperations(fc, ctxt.toplevelCrdtOperations)
        typed.DatabaseCall(fc.source, t, op)
      case _ =>
        // TODO check if expected type is for some operation
        //  checkFunctionCallForExpectedOperations(fc, operations)


        val definition = lookup(fc.functionName)
        val (newKind, t, typedArgs) = definition.typ match {
          case typed.FunctionType(argTypes, returnType, kind) =>



            val typedArgs = for ((arg, expectedArgType) <- fc.args.zip(argTypes.toStream ++ Stream.continually(typed.AnyType()))) yield
              checkExpr(arg, expectedArgType)

            checkCall(fc.source, typedArgs, argTypes)
            (kind, resolveDependentReturn(returnType, typedArgs.map(_.getTyp)), typedArgs)
          case AnyType() =>
            (FunctionKindDatatypeConstructor(), AnyType(), fc.args.map(checkExpr(_)))
          case tt =>
            addError(fc.functionName, s"${fc.functionName.name} of type $tt is not a function.")
            (FunctionKindDatatypeConstructor(), AnyType(), fc.args.map(checkExpr(_)))
        }
        typed.FunctionCall(
          fc.source,
          t,
          definition.name,
          typedArgs,
          newKind
        )
    }
  }

  private def checkFunctionCallForExpectedOperations(fc: FunctionCall, operations: List[Operation])(implicit ctxt: Context): typed.FunctionCall = {
    operations.find(_.name.originalName == fc.functionName.name) match {
      case Some(op) =>
        val argTypes = op.paramTypes
        if (fc.args.size < argTypes.size)
          addError(fc, s"Not enough arguments, expected ${argTypes.mkString(", ")}")
        if (fc.args.size > argTypes.size)
          addError(fc, s"Too many arguments, expected ${argTypes.mkString(", ")}")

        val typedArgs = for ((arg, expectedArgType) <- fc.args.zip(argTypes)) yield
          checkExpr(arg, expectedArgType)

        val returnType = resolveDependentReturn(op.queryReturnType, typedArgs.map(_.getTyp))
        val instance = op.crdtInstance
        val t = typed.OperationType(op.name, returnType)()


        typed.FunctionCall(
          fc.source,
          t,
          op.name,
          typedArgs,
          FunctionKindDatatypeConstructor()
        )
      case None =>
        addError(fc, s"Could not find operation ${fc.functionName}\nSimilar operations: ${suggestNamesStr(fc.functionName.name, operations.map(_.name.originalName))}")
        ???
    }
  }
}

object Typer {

  class TypeErrorException(trace: SourceTrace, msg: String) extends RuntimeException(s"Error in line ${trace.getLine}: $msg") {
  }

}


