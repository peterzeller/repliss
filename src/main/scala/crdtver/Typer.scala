package crdtver

import crdtver.InputAst._


sealed abstract class Type {

}


class Typer {


  case class Context(
    types: Map[String, InTypeExpr],
    declaredTypes: Map[String, InTypeExpr],
    expectedReturn: Option[InTypeExpr] = None
  )

  def addError(query: AstElem, msg: String): Unit = {
    val line = query.getSource().getLine()

    throw new RuntimeException(s"Error in line $line: $msg")
  }



  def checkProgram(program: InProgram): InProgram = {
    var nameBindings = Map[String, InTypeExpr]()
    var declaredTypes = Map[String, InTypeExpr](
      "boolean" -> BoolType(),
      "int" -> IntType(),
      "callId" -> CallIdType(),
      "invocationId" -> InvocationIdType(),
      "invocationInfo" -> InvocationInfoType()
    )

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
      for (c <- t.dataTypeCases) {
        nameBindings += (c.name.name -> FunctionType(c.params.map(_.typ), SimpleType(name)))
      }
    }

    for (p <- program.procedures) {
      var paramTypes: List[InTypeExpr] = p.params.map(_.typ)
      p.returnType match {
        case Some(t) =>
          paramTypes = paramTypes ++ List(t)
        case None =>
      }
      nameBindings += (p.name.name -> FunctionType(paramTypes, InvocationInfoType()))
    }

    val preContext = Context(
      types = nameBindings,
      declaredTypes = declaredTypes
    )

    nameBindings = Map() ++ (for ((n,t) <- nameBindings) yield n -> checkType(t)(preContext))

    implicit val baseContext = Context(
      types = nameBindings,
      declaredTypes = declaredTypes
    )

    for ((n,t) <- nameBindings) {
      println(s"$n --> $t")
    }

    program.copy(
      procedures = program.procedures.map(checkProcedure),
      types = program.types.map(checkTypeDecl),
      operations = program.operations.map(checkOperation),
      queries = program.queries.map(checkQuery),
      axioms = program.axioms.map(checkAxiom),
      invariants = program.invariants.map(checkInvariant)
    )
  }

  def checkProcedure(p: InProcedure)(implicit ctxt: Context): InProcedure = {
    val vars: List[InVariable] = p.params ++ p.locals
    val typesWithParams = ctxt.types ++ getArgTypes(vars)
    val newCtxt = ctxt.copy(
      types = typesWithParams,
      expectedReturn = p.returnType
    )

    p.copy(
      body = checkStatement(p.body)(newCtxt)
    )
  }

  def getArgTypes(vars: List[InVariable])(implicit ctxt: Context): List[(String, InTypeExpr)] = {
    for (param <- vars) yield param.name.name -> checkType(param.typ)
  }

  def checkTypeDecl(t: InTypeDecl)(implicit ctxt: Context): InTypeDecl = {
    // TODO checks necessary?
    t
  }

  def checkType(t: InTypeExpr)(implicit ctxt: Context): InTypeExpr = t match {
    case UnresolvedType(name, _) =>
      ctxt.declaredTypes.getOrElse(name, {
        addError(t, s"Could not find type $name.")
        AnyType()
      })
    case f: FunctionType =>
      f.copy(f.argTypes.map(checkType), checkType(f.returnType))
    case _ => t
  }

  def checkOperation(o: InOperationDecl)(implicit ctxt: Context): InOperationDecl = o

  def checkQuery(q: InQueryDecl)(implicit ctxt: Context): InQueryDecl = {
    lazy val newCtxt = ctxt.copy(
      types = ctxt.types ++ getArgTypes(q.params)
    )
    q.copy(
      implementation = q.implementation.map(checkExpr(_)(newCtxt))
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
      val t: InTypeExpr = ctxt.declaredTypes.getOrElse(typename.name, {
        addError(typename, s"Type ${typename.name} could not be found")
        AnyType()
      })

      if (!t.isInstanceOf[IdType]) {
        addError(typename, s"Type $t must be declared as idType.")
      }
      if (!t.isSubtypeOf(varType)) {
        addError(typename, s"Cannot assign id $t to variable of type $varType.")
      }
      NewIdStmt(source, varname, typename)
    case ReturnStmt(source, expr) =>
      val typedExpr = checkExpr(expr)
      ReturnStmt(source, typedExpr)
  }

  def checkExpr(e: InExpr)(implicit ctxt: Context): InExpr = e match {
    case v @ VarUse(source, typ, name) =>
      val t = ctxt.types.getOrElse[InTypeExpr](name, {
        addError(e, s"Could not find declaration of $name.")
        AnyType()
      })
      v.copy(typ = t)
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
        case BF_getOrigin() =>
          List(CallIdType()) -> InvocationIdType()
      }
      checkCall(ab, typedArgs, argTypes)
      ab.copy(typ = t, args = typedArgs)
    case qe @ QuantifierExpr(source, _, quantifier, vars, expr) =>
      val newCtxt = ctxt.copy(
        types = ctxt.types ++ getArgTypes(vars)
      )
      val exprTyped = checkExpr(expr)(newCtxt)
      if (!exprTyped.getTyp.isSubtypeOf(BoolType())) {
          addError(expr, s"Expression inside quantifier expression must be boolean, but type was ${exprTyped.getTyp}.")
      }
      qe.copy(typ = BoolType(), expr = exprTyped)
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
