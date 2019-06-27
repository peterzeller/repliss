package crdtver.language

import crdtver.Repliss
import crdtver.Repliss._
import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst._
import crdtver.language.TypedAst.FunctionKind.{FunctionKindCrdtQuery, FunctionKindDatatypeConstructor}
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, CrdtTypeDefinitionType, DatabaseCall, FunctionKind, IdType, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType, TransactionIdType}
import crdtver.language.crdts.{CrdtContext, CrdtInstance, CrdtTypeDefinition, UniqueName}
import crdtver.language.{TypedAst => typed}
import crdtver.utils.{Err, Ok}
import info.debatty.java.stringsimilarity.JaroWinkler

/**
  * Code for typing an InputProgram.
  *
  * checkProgram returns an AST annotated with types.
  */
class Typer {

  private var errors: List[Error] = List[Repliss.Error]()

  trait TypeContext {
    def declaredTypes: Map[String, typed.InTypeExpr]
  }

  case class TypeContextImpl(declaredTypes: Map[String, typed.InTypeExpr]) extends TypeContext


  case class Context(
    // types of variables
    types: Map[String, typed.InTypeExpr],
    declaredTypes: Map[String, typed.InTypeExpr],
    datatypes: Map[String, Map[String, typed.FunctionType]],
    // expected return value of current procedure
    expectedReturn: Option[typed.InTypeExpr] = None,
    //
    crdtContext: CrdtContext
  ) extends TypeContext {
    def withBinding(varname: String, typ: typed.InTypeExpr): Context = {
      copy(
        types = types + (varname -> typ)
      )
    }
  }

  def splitEitherList[A, B](el: List[Either[A, B]]): (List[A], List[B]) = {
    val (lefts, rights) = el.partition(_.isLeft)
    (lefts.map(_.left.get), rights.map(_.right.get))
  }

  def toInstance(c: InCrdtType)(implicit ctxt: Context): Either[CrdtInstance, typed.InTypeExpr] = {
    c match {
      case InCrdt(source, name, typ) =>
        val list: List[Either[CrdtInstance, typed.InTypeExpr]] = typ.map(toInstance)
        val (crdtArgs, typeArgs) = splitEitherList(list)
        for (crdt <- CrdtTypeDefinition.crdts) {
          if (crdt.name == name.name) {
            crdt.makeInstance(typeArgs, crdtArgs, ctxt.crdtContext) match {
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
          toInstance(key.crdttype) match {
            case Left(a) =>
              val name = ctxt.crdtContext.newName(key.name.name)
              fields += (name -> a)
            case Right(b) => println("Invalid arguments given")
          }
        }
        return Left(StructInstance(fields, ctxt.crdtContext))
    }
  }

  def crdtunfold(nameBindings: Map[String, typed.InTypeExpr], key: InKeyDecl)(implicit ctxt: Context): Map[String, typed.InTypeExpr] = {
    var tempBindings = nameBindings
    toInstance(key.crdttype) match {
      case Left(a) =>
        for (op <- a.operations) {
          val opName = key.name.name + '_' + op.name
          if (tempBindings.contains(opName)) {
            addError(key.crdttype, s"Element with name $opName already exists.")
          }
          tempBindings += (opName -> typed.FunctionType(op.paramTypes, typed.OperationType(opName)(), FunctionKindDatatypeConstructor())())
        }
      case Right(AnyType()) =>
        // avoid cascading errors
      case Right(b) =>
        addError(key.crdttype, "Invalid type: " + b)
    }
    return tempBindings
  }

  def addError(elem: AstElem, msg: String): Unit = {
    val source = elem.getErrorSource
    addError(source, msg)
  }

  private def addError(source: SourceTrace, msg: String): Unit = {
    val err = Error(source.range, msg)
    errors :+= err
  }

  def addError(elem: typed.AstElem, msg: String): Unit = {
    val source = elem.getErrorSource
    addError(source, msg)
  }

  def checkProgram(program: InProgram): Result[typed.InProgram] = {
    var nameBindings = Map[String, typed.InTypeExpr](
      "NoResult" -> typed.FunctionType(List(), typed.InvocationResultType(), typed.FunctionKind.FunctionKindDatatypeConstructor())()
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
      declaredTypes += (name -> OperationType(name)())
    }

    for (t <- program.types) {
      val name = t.name.name
      if (declaredTypes.contains(name)) {
        addError(t, s"Element with name $name already exists.")
      }

      if (t.isIdType) {
        declaredTypes += (name -> IdType(name)())
        if (t.dataTypeCases.nonEmpty) {
          addError(t, s"Id type $name cannot be a datatype.")
        }
      } else {
        declaredTypes += (name -> SimpleType(name)())
      }
    }

    for (crdt <- CrdtTypeDefinition.crdts) {
      declaredTypes += (crdt.name -> CrdtTypeDefinitionType(crdt))
    }


    var datatypes = Map[String, Map[String, typed.FunctionType]]()

    val crdtContext = new CrdtContext()

    val typeContext = TypeContextImpl(declaredTypes)


    // build toplevel context:
    for (query <- program.queries) {
      val name = query.name.name
      if (nameBindings contains name) {
        addError(query, s"Element with name $name already exists.")
      }
      nameBindings += (name -> typed.FunctionType(query.params.map(t => checkType(t.typ)(typeContext)), checkType(query.returnType)(typeContext), FunctionKindCrdtQuery())())
    }

    for (operation <- program.operations) {
      val name = operation.name.name
      nameBindings += (name -> typed.FunctionType(operation.params.map(t => checkType(t.typ)(typeContext)), OperationType(name)(), FunctionKindDatatypeConstructor())())
    }


    for (t <- program.types) {
      val name = t.name.name
      val dtCases: List[(String, typed.FunctionType)] =
        for (c <- t.dataTypeCases) yield {
          c.name.name -> typed.FunctionType(c.params.map(t => checkType(t.typ)(typeContext)), SimpleType(name)(), typed.FunctionKind.FunctionKindDatatypeConstructor())()
        }
      if (dtCases.nonEmpty) {
        nameBindings = nameBindings ++ dtCases
        datatypes += name -> (Map() ++ dtCases)
      }
    }


    for (crdt <- program.crdts) {
      implicit val typeCtxt: Context = Context(
        types = nameBindings,
        declaredTypes = declaredTypes,
        datatypes = datatypes,
        crdtContext = crdtContext
      )
      nameBindings = nameBindings ++ crdtunfold(nameBindings, crdt.keyDecl)
    }

    for (p <- program.procedures) {
      val paramTypes: List[InTypeExpr] = p.params.map(_.typ)
      // invocation info constructor
      nameBindings += (p.name.name -> typed.FunctionType(paramTypes.map(checkType(_)(typeContext)), InvocationInfoType(), FunctionKindDatatypeConstructor())())
      // invocation result constructor
      nameBindings += (s"${p.name.name}_res" -> typed.FunctionType(p.returnType.map(checkType(_)(typeContext)).toList, InvocationResultType(), FunctionKindDatatypeConstructor())())
    }

    val preContext = Context(
      types = nameBindings,
      declaredTypes = declaredTypes,
      datatypes = datatypes,
      crdtContext = crdtContext
    )

    implicit val baseContext = preContext.copy(
      types = nameBindings,
      datatypes = datatypes
    )

    var crdts = Map[UniqueName, CrdtInstance]()
    for (crdt <- program.crdts) {
      toInstance(crdt.keyDecl.crdttype) match {
        case Left(instance) =>
          val name = crdtContext.newName(crdt.keyDecl.name.name)
          crdts += name -> instance
        case Right(_) =>
      }
    }


    val checkedProgram = typed.InProgram(
      name = program.name,
      source = program.source,
      procedures = program.procedures.map(checkProcedure),
      types = program.types.map(checkTypeDecl),
      axioms = program.axioms.map(checkAxiom),
      invariants = program.invariants.map(checkInvariant),
      programCrdt = StructInstance(fields = crdts, crdtContext)
    )

    if (errors.isEmpty) {
      NormalResult(checkedProgram)
    } else {
      ErrorResult(errors)
    }
  }


  def checkProcedure(p: InProcedure)(implicit ctxt: Context): typed.InProcedure = {
    val vars: List[InVariable] = p.params ++ p.locals
    val typesWithParams = ctxt.types ++ getArgTypes(vars)
    val newCtxt = ctxt.copy(
      types = typesWithParams,
      expectedReturn = p.returnType.map(checkType)
    )

    typed.InProcedure(
      name = p.name,
      source = p.source,
      body = checkStatement(p.body)(newCtxt),
      params = checkParams(p.params),
      locals = checkParams(p.locals),
      returnType = p.returnType.map(checkType)
    )
  }

  def getArgTypesT(vars: List[typed.InVariable]): List[(String, typed.InTypeExpr)] = {
    for (param <- vars) yield param.name.name -> param.typ
  }

  def getArgTypes(vars: List[InVariable])(implicit ctxt: Context): List[(String, typed.InTypeExpr)] = {
    for (param <- vars) yield param.name.name -> checkType(param.typ)
  }

  def checkTypeDecl(t: InTypeDecl)(implicit ctxt: Context): typed.InTypeDecl = {
    // TODO checks necessary?
    typed.InTypeDecl(
      name = t.name,
      source = t.source,
      isIdType = t.isIdType,
      dataTypeCases = t.dataTypeCases.map(c => typed.DataTypeCase(
        name = c.name,
        source = c.source,
        params = checkParams(c.params)
      ))
    )
  }

  def checkVariable(variable: InVariable)(implicit ctxt: Context): typed.InVariable =
    typed.InVariable(name = variable.name, source = variable.source, typ = checkType(variable.typ))

  def checkParams(params: List[InVariable])(implicit ctxt: Context): List[typed.InVariable] = {
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

  def checkFunctionType(f: FunctionType, kind: FunctionKind)(implicit ctxt: TypeContext): typed.FunctionType = {
    typed.FunctionType(f.argTypes.map(checkType), checkType(f.returnType), kind)(source = f.getSource)
  }

  //  def checkOperation(o: InOperationDecl)(implicit ctxt: Context): typed.InOperationDecl = {
  //    typed.InOperationDecl()
  //    o.copy(params = checkParams(o.params))
  //  }

  def checkQuery(q: InQueryDecl)(implicit ctxt: Context): typed.InQueryDecl = {
    lazy val newCtxt = ctxt.copy(
      types = ctxt.types ++ getArgTypes(q.params)
    )
    val returnType = checkType(q.returnType)

    lazy val ensuresCtxt = newCtxt.copy(
      types = newCtxt.types + ("result" -> returnType)
    )
    typed.InQueryDecl(
      name = q.name,
      source = q.source,
      annotations = q.annotations,
      implementation = q.implementation.map(checkExpr(_)(newCtxt)),
      ensures = q.ensures.map(checkExpr(_)(ensuresCtxt)),
      params = checkParams(q.params),
      returnType = returnType
    )
  }

  def checkAxiom(p: InAxiomDecl)(implicit ctxt: Context): typed.InAxiomDecl =
    typed.InAxiomDecl(
      source = p.source,
      expr = checkExpr(p.expr)
    )

  def checkInvariant(i: InInvariantDecl)(implicit ctxt: Context): typed.InInvariantDecl =
    typed.InInvariantDecl(
      source = i.source,
      expr = checkExpr(i.expr)
    )


  def lookup(varname: Identifier)(implicit ctxt: Context): typed.InTypeExpr =
    lookup(varname.name, varname.source)

  def lookup(varname: String, source: SourceTrace)(implicit ctxt: Context): typed.InTypeExpr = {
    ctxt.types.getOrElse[typed.InTypeExpr](varname, {
      addError(source, s"Could not find declaration of $varname.\nSimilar definitions: ${suggestNamesStr(varname, ctxt.types.keys)}")
      AnyType()
    })
  }

  def checkStatement(s: InStatement)(implicit ctxt: Context): typed.InStatement = s match {
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
      val callTyped = checkFunctionCall(call)
      callTyped match {
        case DatabaseCall(source, typ, instance, operation) =>
          typed.CrdtCall(source, None, instance, operation)
        case _ =>
          addError(call, s"Not an operation.")
          typed.makeBlock(source, List())
      }
    case Assignment(source, varname, expr) =>
      val varType: typed.InTypeExpr = lookup(varname)
      val exprTyped = checkExpr(expr)
      if (!exprTyped.getTyp.isSubtypeOf(varType)) {
        addError(expr, s"Expression of type ${exprTyped.getTyp} is not assignable to variable of type $varType.")
      }
      typed.Assignment(source, varname, exprTyped)
    case NewIdStmt(source, varname, typename) =>
      val varType: typed.InTypeExpr = lookup(varname)
      val t = checkType(typename)

      if (!t.isInstanceOf[IdType]) {
        addError(typename, s"Type $t must be declared as idType.")
      }
      if (!t.isSubtypeOf(varType)) {
        addError(typename, s"Cannot assign id $t to variable of type $varType.")
      }
      typed.NewIdStmt(source, varname, t)
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

      val assertionCtxt = ctxt.withBinding("newInvocationId", InvocationIdType())
      typed.ReturnStmt(source, typedExpr, assertions.map(checkAssertStatement(_)(assertionCtxt)))
    case s: AssertStmt =>
      checkAssertStatement(s)
  }

  def checkAssertStatement(s: AssertStmt)(implicit ctxt: Context): typed.AssertStmt = {
    typed.AssertStmt(source = s.source, expr = checkExpr(s.expr))
  }


  def checkCase(c: MatchCase, expectedType: typed.InTypeExpr)(implicit ctxt: Context): typed.MatchCase = {
    val (newCtxt, patternTyped) = checkPattern(c.pattern, expectedType)(ctxt)
    val stmtTyped = checkStatement(c.statement)(newCtxt)
    typed.MatchCase(
      source = c.source,
      pattern = patternTyped,
      statement = stmtTyped
    )
  }

  def checkPattern(pattern: InExpr, expectedType: typed.InTypeExpr)(implicit ctxt: Context): (Context, typed.InExpr) = pattern match {
    case VarUse(source, name) =>
      if (ctxt.types.contains(name)) {
        addError(pattern, s"Variable with name $name is already defined.")
      }
      val newCtxt = ctxt.withBinding(name, expectedType)
      (newCtxt, typed.VarUse(source, expectedType, name))
    case f@FunctionCall(source, functionName, args) =>
      var newCtxt = ctxt
      expectedType match {
        case SimpleType(dtName) =>
          ctxt.datatypes.get(dtName) match {
            case None =>
              addError(pattern, s"Type $expectedType is not a datatype.")
            case Some(dtInfo) =>
              dtInfo.get(functionName.name) match {
                case None =>
                  addError(pattern, s"$functionName is not a case of type $expectedType")
                case Some(functionType) =>
                  if (functionType.argTypes.size != args.size) {
                    addError(pattern, s"Expected (${functionType.argTypes.mkString(",")}) arguments, but got (${args.mkString(",")}")
                  }
                  val typedArgs = for ((a, t) <- args.zip(functionType.argTypes)) yield {
                    val (c, argTyped) = checkPattern(a, t)(newCtxt)
                    newCtxt = c
                    argTyped
                  }
                  return (newCtxt, typed.FunctionCall(
                    source,
                    expectedType,
                    functionName,
                    typedArgs,
                    FunctionKindDatatypeConstructor()
                  ))
              }

          }
        case _ =>
          addError(f, s"Cannot use functionCall with expected Type $expectedType.")
          ???

      }
      println(s"check $pattern with type ${expectedType.getClass}")
      ???
    case f =>
      addError(pattern, s"Pattern not supported: $pattern")
      (ctxt, typed.VarUse(f.getSource, AnyType(), "_"))
  }

  def checkExpr(e: InExpr)(implicit ctxt: Context): typed.InExpr = e match {
    case v@VarUse(source, name) =>
      val t = lookup(name, source)
      typed.VarUse(source, t, name)
    case BoolConst(source, v) =>
      typed.BoolConst(source, BoolType(), v)
    case IntConst(source, value) =>
      typed.IntConst(source, IntType(), value)
    case fc: FunctionCall =>
      checkFunctionCall(fc)
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
      val typedArgs = args.map(checkExpr)
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
          val t1 = typedArgs(0).getTyp
          val t2 = typedArgs(1).getTyp
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


  def checkCall(source: SourceTrace, typedArgs: List[typed.InExpr], argTypes: List[typed.InTypeExpr]): Unit = {
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

  def checkFunctionCall(fc: FunctionCall)(implicit ctxt: Context): typed.CallExpr = {
    val typedArgs = fc.args.map(checkExpr)
    val (newKind, t) = lookup(fc.functionName) match {
      case typed.FunctionType(argTypes, returnType, kind) =>
        checkCall(fc.source, typedArgs, argTypes)
        (kind, returnType)
      case AnyType() =>
        (FunctionKindDatatypeConstructor(), AnyType())
      case _ =>
        addError(fc.functionName, s"${fc.functionName.name} is not a function.")
        (FunctionKindDatatypeConstructor(), AnyType())
    }
    typed.FunctionCall(
      fc.source,
      t,
      fc.functionName,
      typedArgs,
      newKind
    )
  }


}

object Typer {

  class TypeErrorException(trace: SourceTrace, msg: String) extends RuntimeException(s"Error in line ${trace.getLine}: $msg") {
  }

}


