package crdtver.language

import crdtver.Repliss
import crdtver.Repliss._
import crdtver.language.InputAst._

/**
  * Code for typing an InputProgram.
  *
  * checkProgram returns an AST annotated with types.
  */
class Typer {

  private var errors: List[Error] = List[Repliss.Error]()


  case class Context(
    types: Map[String, InTypeExpr],
    declaredTypes: Map[String, InTypeExpr],
    datatypes: Map[String, Map[String, FunctionType]],
    expectedReturn: Option[InTypeExpr] = None
  ) {
    def withBinding(varname: String, typ: InTypeExpr): Context = {
      copy(
        types = types + (varname -> typ)
      )
    }
  }

  def crdtunfold(key: InKeyDecl, prefix: String, argstype: List[InTypeExpr]): Map[String, InTypeExpr] = {
    var nameBindings = Map[String, InTypeExpr]()
    key.crdttype match {
      case InCrdt(source, name, typ) =>
        name.name match {
          case "Register" =>
            val opassign = prefix + key.name.name + "_assign"
            val opget = prefix + key.name.name + "_get"
            val opType = argstype ++ typ
            nameBindings += (opassign -> FunctionType(opType, OperationType(opassign)))
            nameBindings += (opget -> FunctionType(argstype, typ.head))
          case "Set_aw" =>
            val opadd = prefix + key.name.name + "_add"
            val opremove = prefix + key.name.name + "_remove"
            val opcontains = prefix + key.name.name + "_contains"
            val opType = argstype ++ typ
            nameBindings += (opadd -> FunctionType(opType, OperationType(opadd)))
            nameBindings += (opremove -> FunctionType(opType, OperationType(opremove)))
            nameBindings += (opcontains -> FunctionType(opType, BoolType()))
        }
      case InMapCrdt(source, typ, keyDecl) =>
        for(elem <- keyDecl.iterator) {
          var newprefix = key.name.name + "_"
          var newargstype = List(typ)
          nameBindings = nameBindings ++ crdtunfold(elem, newprefix, newargstype)
        }
    }
    return nameBindings
  }

  def addError(elem: AstElem, msg: String): Unit = {
    val source = elem.getSource()
    errors :+= Error(source.range, msg)
  }

  def checkProgram(program: InProgram): Result[InProgram] = {
    var nameBindings = Map[String, InTypeExpr](
      "NoResult" -> FunctionType(List(), InvocationResultType())
    )
    var declaredTypes = Map[String, InTypeExpr](
      "boolean" -> BoolType(),
      "int" -> IntType(),
      "callId" -> CallIdType(),
      "invocationId" -> InvocationIdType(),
      "invocationInfo" -> InvocationInfoType()
    )
    var datatypes = Map[String, Map[String, FunctionType]]()

    // build toplevel context:
    for (query <- program.queries) {
      val name = query.name.name
      if (nameBindings contains name) {
        addError(query, s"Element with name $name already exists.")
      }
      nameBindings += (name -> FunctionType(query.params.map(_.typ), query.returnType))
    }

    for (operation <- program.operations) {
      val name = operation.name.name
      if (nameBindings.contains(name) || declaredTypes.contains(name)) {
        addError(operation, s"Element with name $name already exists.")
      }
      nameBindings += (name -> FunctionType(operation.params.map(_.typ), OperationType(name)))
      declaredTypes += (name -> OperationType(name))
    }

    for(crdt <- program.crdts) {
      val name = crdt.keyDecl.name.name
      if (nameBindings.contains(name) || declaredTypes.contains(name)) {
        addError(crdt, s"Element with name $name already exists.")
      }
      nameBindings = nameBindings ++ crdtunfold(crdt.keyDecl,"",List())
    }

    for (t <- program.types) {
      val name = t.name.name
      if (declaredTypes.contains(name)) {
        addError(t, s"Element with name $name already exists.")
      }

      if (t.isIdType) {
        declaredTypes += (name -> IdType(name))
        if (t.dataTypeCases.nonEmpty) {
          addError(t, s"Id type $name cannot be a datatype.")
        }
      } else {
        declaredTypes += (name -> SimpleType(name))
      }
      val dtCases = for (c <- t.dataTypeCases) yield {
        c.name.name -> FunctionType(c.params.map(_.typ), SimpleType(name))
      }
      if (dtCases.nonEmpty) {
        nameBindings = nameBindings ++ dtCases
        datatypes += name -> (Map() ++ dtCases)
      }
    }

    for (p <- program.procedures) {
      val paramTypes: List[InTypeExpr] = p.params.map(_.typ)
      // invocation info constructor
      nameBindings += (p.name.name -> FunctionType(paramTypes, InvocationInfoType()))
      // invocation result constructor
      nameBindings += (s"${p.name.name}_res" -> FunctionType(p.returnType.toList, InvocationResultType()))
    }

    val preContext = Context(
      types = nameBindings,
      declaredTypes = declaredTypes,
      datatypes = datatypes
    )

    nameBindings = Map() ++ (for ((n,t) <- nameBindings) yield n -> checkType(t)(preContext))
    datatypes = Map() ++ (for ((n, cases) <- datatypes) yield {
      n -> (Map() ++ (for ((caseName, ft) <- cases) yield caseName -> checkFunctionType(ft)(preContext)))
    })



    implicit val baseContext = preContext.copy(
      types = nameBindings,
      datatypes = datatypes
    )

    val checkedProgram = program.copy(
      procedures = program.procedures.map(checkProcedure),
      types = program.types.map(checkTypeDecl),
      operations = program.operations.map(checkOperation),
      queries = program.queries.map(checkQuery),
      axioms = program.axioms.map(checkAxiom),
      invariants = program.invariants.map(checkInvariant)
    )

    if (errors.isEmpty) {
      NormalResult(checkedProgram)
    } else {
      ErrorResult(errors)
    }
  }



  def checkProcedure(p: InProcedure)(implicit ctxt: Context): InProcedure = {
    val vars: List[InVariable] = p.params ++ p.locals
    val typesWithParams = ctxt.types ++ getArgTypes(vars)
    val newCtxt = ctxt.copy(
      types = typesWithParams,
      expectedReturn = p.returnType
    )

    p.copy(
      body = checkStatement(p.body)(newCtxt),
      params = checkParams(p.params),
      locals = checkParams(p.locals),
      returnType = p.returnType.map(checkType)
    )
  }

  def getArgTypes(vars: List[InVariable])(implicit ctxt: Context): List[(String, InTypeExpr)] = {
    for (param <- vars) yield param.name.name -> checkType(param.typ)
  }

  def checkTypeDecl(t: InTypeDecl)(implicit ctxt: Context): InTypeDecl = {
    // TODO checks necessary?
    t.copy(
      dataTypeCases = t.dataTypeCases.map(c => c.copy(
        params = checkParams(c.params)
      ))
    )
  }

  def checkVariable(variable: InVariable)(implicit ctxt: Context): InVariable =
    variable.copy(typ = checkType(variable.typ))

  def checkParams(params: List[InVariable])(implicit ctxt: Context): List[InVariable] = {
    params.map(checkVariable)
  }


  def checkType(t: InTypeExpr)(implicit ctxt: Context): InTypeExpr = t match {
    case UnresolvedType(name, _) =>
      ctxt.declaredTypes.getOrElse(name, {
        addError(t, s"Could not find type $name.")
        AnyType()
      })
    case f: FunctionType =>
      checkFunctionType(f)
    case _ => t
  }

  def checkFunctionType(f: FunctionType)(implicit ctxt: Context): FunctionType = {
    f.copy(f.argTypes.map(checkType), checkType(f.returnType))
  }

  def checkOperation(o: InOperationDecl)(implicit ctxt: Context): InOperationDecl = {
    o.copy(params = checkParams(o.params))
  }

  def checkQuery(q: InQueryDecl)(implicit ctxt: Context): InQueryDecl = {
    lazy val newCtxt = ctxt.copy(
      types = ctxt.types ++ getArgTypes(q.params)
    )
    val returnType = checkType(q.returnType)

    lazy val ensuresCtxt = newCtxt.copy(
      types = newCtxt.types + ("result" -> returnType)
    )
    q.copy(
      implementation = q.implementation.map(checkExpr(_)(newCtxt)),
      ensures = q.ensures.map(checkExpr(_)(ensuresCtxt)),
      params = checkParams(q.params),
      returnType = returnType
    )
  }

  def checkAxiom(p: InAxiomDecl)(implicit ctxt: Context): InAxiomDecl =
    p.copy(
      expr = checkExpr(p.expr)
    )

  def checkInvariant(i: InInvariantDecl)(implicit ctxt: Context): InInvariantDecl =
    i.copy(
      expr = checkExpr(i.expr)
    )


  def lookup(varname: Identifier)(implicit ctxt: Context): InTypeExpr = {
    ctxt.types.getOrElse[InTypeExpr](varname.name, {
      addError(varname, s"Could not find declaration of ${varname.name}.")
      AnyType()
    })
  }

  def checkStatement(s: InStatement)(implicit ctxt: Context): InStatement = s match {
    case BlockStmt(source, stmts) =>
      BlockStmt(source, stmts.map(checkStatement))
    case Atomic(source, body) =>
      Atomic(source, checkStatement(body))
    case LocalVar(source, variable) =>
      LocalVar(source, variable)
    case IfStmt(source, cond, thenStmt, elseStmt) =>
      val condTyped: InExpr = checkExpr(cond)
      if (!condTyped.getTyp.isSubtypeOf(BoolType())) {
        addError(cond, s"Expression of if-statement must be boolean, but was ${condTyped.getTyp}.")
      }
      IfStmt(source, condTyped, checkStatement(thenStmt), checkStatement(elseStmt))
    case MatchStmt(source, expr, cases) =>
      val exprTyped = checkExpr(expr)

      val casesTyped = cases.map(checkCase(_, exprTyped.getTyp))

      MatchStmt(source, exprTyped, casesTyped)
    case CrdtCall(source, call) =>
      val callTyped = checkFunctionCall(call)
      if (!callTyped.getTyp.isSubtypeOf(SomeOperationType())) {
        addError(call, s"Not an operation.")
      }
      CrdtCall(source, callTyped)
    case Assignment(source, varname, expr) =>
      val varType: InTypeExpr = lookup(varname)
      val exprTyped = checkExpr(expr)
      if (!exprTyped.getTyp.isSubtypeOf(varType)) {
        addError(expr, s"Expression of type ${exprTyped.getTyp} is not assignable to variable of type $varType.")
      }
      Assignment(source, varname, exprTyped)
    case NewIdStmt(source, varname, typename) =>
      val varType: InTypeExpr = lookup(varname)
      val t = checkType(typename)

      if (!t.isInstanceOf[IdType]) {
        addError(typename, s"Type $t must be declared as idType.")
      }
      if (!t.isSubtypeOf(varType)) {
        addError(typename, s"Cannot assign id $t to variable of type $varType.")
      }
      NewIdStmt(source, varname, t)
    case ReturnStmt(source, expr, assertions) =>
      val typedExpr = checkExpr(expr)
      val assertionCtxt = ctxt.withBinding("newInvocationId", InvocationIdType())
      ReturnStmt(source, typedExpr, assertions.map(checkAssertStatement(_)(assertionCtxt)))
    case s: AssertStmt =>
      checkAssertStatement(s)
  }

  def checkAssertStatement(s: AssertStmt)(implicit ctxt: Context): AssertStmt = {
    s.copy(expr = checkExpr(s.expr))
  }


  def checkCase(c: MatchCase, expectedType: InTypeExpr)(implicit ctxt: Context): MatchCase = {
    val (newCtxt, patternTyped) = checkPattern(c.pattern, expectedType)(ctxt)
    val stmtTyped = checkStatement(c.statement)(newCtxt)
    c.copy(
      pattern = patternTyped,
      statement = stmtTyped
    )
  }

  def checkPattern(pattern: InExpr, expectedType: InTypeExpr)(implicit ctxt: Context): (Context, InExpr) = pattern match {
    case v @ VarUse(source, typ, name) =>
      if (ctxt.types.contains(name)) {
        addError(pattern, s"Variable with name $name is already defined.")
      }
      val newCtxt = ctxt.withBinding(name, expectedType)
      (newCtxt, v.copy(typ = expectedType))
    case f @ FunctionCall(source, typ, functionName, args) =>
      var newCtxt = ctxt
      expectedType match {
        case SimpleType(dtName, _) =>
          ctxt.datatypes.get(dtName) match {
            case None =>
              addError(pattern, s"Type $expectedType is not a datatype.")
            case Some(dtInfo) =>
              dtInfo.get(functionName.name) match {
                case None =>
                  addError(pattern, s"$functionName is not a case of type $expectedType")
                case Some(functionType) =>
                  if (functionType.argTypes.size != args.size) {
                    addError(pattern, s"Expected ${functionType.argTypes.size} arguments, but got ${args.size}")
                  }
                  val typedArgs = for ((a, t) <- args.zip(functionType.argTypes)) yield {
                    val (c, argTyped) = checkPattern(a, t)(newCtxt)
                    newCtxt = c
                    argTyped
                  }
                  return (newCtxt, f.copy(
                    typ = expectedType,
                    args = typedArgs
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
      (ctxt, f)
  }

  def checkExpr(e: InExpr)(implicit ctxt: Context): InExpr = e match {
    case v @ VarUse(source, typ, name) =>
      val t = ctxt.types.getOrElse[InTypeExpr](name, {
        addError(e, s"Could not find declaration of $name.")
        AnyType()
      })
      v.copy(typ = t)
    case b: BoolConst =>
      b.copy(typ = BoolType())
    case fc @ FunctionCall(source, typ, functionName, args) =>
      checkFunctionCall(fc)
    case ab @ ApplyBuiltin(source, typ, function, args) =>
      val typedArgs = args.map(checkExpr)
      val (argTypes: List[InTypeExpr], t:InTypeExpr) = function match {
        case BF_isVisible() =>
          List(CallIdType()) -> BoolType()
        case BF_happensBefore() =>
          typedArgs.headOption match {
            case Some(expr) if expr.getTyp.isSubtypeOf(CallIdType()) =>
              List(CallIdType(), CallIdType()) -> BoolType()
            case _ =>
              List(InvocationIdType(), InvocationIdType()) -> BoolType()
          }
        case BF_sameTransaction() =>
          List(CallIdType(), CallIdType()) -> BoolType()
        case BF_less() | BF_lessEq() | BF_greater() | BF_greaterEq() =>
          List(IntType(), IntType()) -> BoolType()
        case BF_equals() | BF_notEquals() =>
          // TODO check that types are equal/comparable
          List(AnyType(), AnyType()) -> BoolType()
        case BF_and() | BF_or() | BF_implies() =>
          List(BoolType(), BoolType()) -> BoolType()
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
        case BF_inCurrentInvoc() =>
          List(CallIdType()) -> BoolType()
      }
      checkCall(ab, typedArgs, argTypes)
      ab.copy(typ = t, args = typedArgs)
    case qe @ QuantifierExpr(source, _, quantifier, vars, expr) =>
      val typedVars = checkParams(vars)
      val newCtxt = ctxt.copy(
        types = ctxt.types ++ getArgTypes(typedVars)
      )
      val exprTyped = checkExpr(expr)(newCtxt)
      if (!exprTyped.getTyp.isSubtypeOf(BoolType())) {
          addError(expr, s"Expression inside quantifier expression must be boolean, but type was ${exprTyped.getTyp}.")
      }
      qe.copy(
        typ = BoolType(),
        vars = typedVars,
        expr = exprTyped)
  }


  def checkCall(source: AstElem, typedArgs: List[InExpr], argTypes: List[InTypeExpr]): Unit = {
    if (argTypes.length != typedArgs.length) {
      addError(source, s"Expected ${argTypes.length} arguments, but ${typedArgs.length} arguments were given.")
    } else {
      for ((et, arg) <- argTypes.zip(typedArgs)) {
        if (!arg.getTyp.isSubtypeOf(et)) {
          addError(arg, s"Expected argument of type $et, but got ${arg.getTyp}.")
        }
      }
    }
  }

  def checkFunctionCall(fc: FunctionCall)(implicit ctxt: Context): FunctionCall = {
    val typedArgs = fc.args.map(checkExpr)
    val t: InTypeExpr = lookup(fc.functionName) match {
      case FunctionType(argTypes, returnType, _) =>
        checkCall(fc, typedArgs, argTypes)
        returnType
      case AnyType() => AnyType()
      case _ =>
        addError(fc.functionName, s"${fc.functionName.name} is not a function.")
        AnyType()
    }

    fc.copy(typ = t, args = typedArgs)
  }



}

object Typer {
  class TypeErrorException(trace: SourceTrace, msg: String) extends RuntimeException(s"Error in line ${trace.getLine}: $msg") {
  }
}


