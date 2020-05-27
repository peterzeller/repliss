package crdtver.language

import crdtver.Repliss
import crdtver.Repliss._
import crdtver.language.ACrdtInstance.StructInstance
import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst._
import crdtver.language.TypedAst.FunctionKind.{FunctionKindCrdtQuery, FunctionKindDatatypeConstructor}
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionKind, IdType, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, PrincipleType, SimpleType, SomeOperationType, TransactionIdType, TypeVarUse, UnitType}
import crdtver.language.Typer.{TypeConstraint, TypeConstraints, TypesEqual}
import crdtver.language.crdts.CrdtTypeDefinition
import crdtver.language.{TypedAst => typed}
import crdtver.utils.ListExtensions
import ListExtensions.ListUtils
import cats.data.State

/**
 * Code for typing an InputProgram.
 *
 * checkProgram returns an AST annotated with types.
 */
class Typer {

  private var errors: List[Error] = List[Repliss.Error]()

  case class TypeFactory(
    arity: Int,
    makeType: List[typed.InTypeExpr] => typed.InTypeExpr
  )

  def noArgsType(t: typed.InTypeExpr): TypeFactory = TypeFactory(0, _ => t)

  trait TypeContext {
    def declaredTypes: Map[String, TypeFactory]
  }

  case class TypeContextImpl(declaredTypes: Map[String, TypeFactory]) extends TypeContext


  case class Context(
    // types of variables
    types: Map[String, PrincipleType],
    declaredTypes: Map[String, TypeFactory],
    datatypes: Map[String, Map[String, typed.FunctionType]],
    // expected return value of current procedure
    expectedReturn: typed.InTypeExpr = UnitType()
  ) extends TypeContext {
    def withBinding(varname: String, typ: typed.InTypeExpr): Context = {
      copy(
        types = types + (varname -> typ)
      )
    }
  }

  /** can only construct T after type constraint solution exists */
  case class LazyBound[T](
    bind: Map[TypeVarUse, typed.InTypeExpr] => T
  )

  case class ExprResult(
    typ: typed.InTypeExpr,
    makeExpr: LazyBound[typed.InExpr]
  ) {
    def bind(subst: Map[TypeVarUse, typed.InTypeExpr]): TypedAst.InExpr =
      makeExpr.bind(subst)
  }

  type TypeResult[T] = State[TypeConstraints, T]


  def splitEitherList[A, B](el: List[Either[A, B]]): (List[A], List[B]) = {
    val (lefts, rights) = el.partition(_.isLeft)
    (lefts.map(_.swap.toOption.get), rights.map(_.toOption.get))
  }

  def toInstance(c: InCrdtType)(implicit ctxt: Context): Either[ACrdtInstance, typed.InTypeExpr] = {
    c match {
      case InCrdt(source, name, typ) =>
        val list: List[Either[ACrdtInstance, typed.InTypeExpr]] = typ.map(toInstance)
        val (crdtArgs, typeArgs) = splitEitherList(list)
        for (crdt <- CrdtTypeDefinition.crdts) {
          if (crdt.name == name.name) {
            if (crdt.numberTypes != typeArgs.size) //check the number of type arguments as per CrdtTypeDefinition
            addError(c, s"${crdt.name} expected ${crdt.numberTypes} arguments but got (${typeArgs}")
            if (crdt.numberInstances != crdtArgs.size) // check number of crdt arguments. crdtArgs 0 for Register and Set, only Maps are nested
            addError(c, s"${crdt.name} expected ${crdt.numberInstances} crdt arguments but got (${crdtArgs}")
            return Left(ACrdtInstance.CrdtInstance(crdt, typeArgs, crdtArgs))
          }
        }

        Right(lookupType(source, name.name, typeArgs))
      case InStructCrdt(source, keyDecl) =>
        var map = Map[String, ACrdtInstance]()
        for (key <- keyDecl.iterator) {
          toInstance(key.crdttype) match {
            case Left(a) => map += (key.name.name -> a)
            case Right(b) => println("Invalid arguments given")
          }
        }
        Left(ACrdtInstance.StructInstance(map))
    }
  }

  def crdtunfold(nameBindings: Map[String, PrincipleType], key: InKeyDecl)(implicit ctxt: Context): Map[String, PrincipleType] = {
    var tempBindings = nameBindings
    toInstance(key.crdttype) match {
      case Left(a) =>
        for (op <- a.operations()) {
          val opName = key.name.name + '_' + op.name
          if (tempBindings.contains(opName)) {
            addError(key.crdttype, s"Element with name $opName already exists.")
          }
          tempBindings += (opName -> typed.FunctionType(op.paramTypes, typed.OperationType(opName)(), FunctionKindDatatypeConstructor())())
        }
        for (q <- a.queries()) {
          val qName = key.name.name + '_' + q.qname
          if (tempBindings.contains(qName)) {
            addError(key.crdttype, s"Element with name $qName already exists.")
          }
          tempBindings += (qName -> typed.FunctionType(q.qparamTypes, q.qreturnType, FunctionKindCrdtQuery())())
          val queryDtName = s"queryop_$qName"
          val queryDtArgs: List[TypedAst.InTypeExpr] = q.qparamTypes :+ q.qreturnType
          tempBindings += (queryDtName -> typed.FunctionType(queryDtArgs, typed.OperationType(queryDtName)(), FunctionKindDatatypeConstructor())())
        }

      case Right(b) =>
        addError(key.crdttype, "Invalid type: " + b)
    }
    tempBindings
  }

  def addError(elem: AstElem, msg: String): Unit = {
    addError(elem.getSource(), msg)
  }

  private def addError(source: SourceTrace, msg: String): Unit = {
    val err = Error(source.range, msg)
    errors :+= err
  }

  def addError(elem: typed.AstElem, msg: String): Unit = {
    addError(elem.getSource(), msg)
  }

  def checkProgram(program: InProgram): Result[typed.InProgram] = {
    var nameBindings = Map[String, PrincipleType](
      "NoResult" -> PrincipleType(List(), typed.FunctionType(List(), typed.InvocationResultType(), FunctionKindDatatypeConstructor())())
    )
    var declaredTypes = Map[String, TypeFactory](
      "boolean" -> noArgsType(typed.BoolType()),
      "int" -> noArgsType(typed.IntType()),
      "callId" -> noArgsType(typed.CallIdType()),
      "invocationId" -> noArgsType(typed.InvocationIdType()),
      "transactionId" -> noArgsType(typed.TransactionIdType()),
      "invocationInfo" -> noArgsType(typed.InvocationInfoType())
    )
    // collect program specific types
    for (operation <- program.operations) {
      val name = operation.name.name
      if (nameBindings.contains(name) || declaredTypes.contains(name)) {
        addError(operation, s"Element with name $name already exists.")
      }
      declaredTypes += (name -> noArgsType(OperationType(name)()))
    }

    for (t <- program.types) {
      val name = t.name.name
      if (declaredTypes.contains(name)) {
        addError(t, s"Element with name $name already exists.")
      }

      if (t.isIdType) {
        declaredTypes += (name -> noArgsType(IdType(name)()))
        if (t.dataTypeCases.nonEmpty) {
          addError(t, s"Id type $name cannot be a datatype.")
        }
      } else {
        val f = TypeFactory(t.typeParameters.length,
          args => SimpleType(name, args)())

        declaredTypes += (name -> f)
      }
    }


    var datatypes = Map[String, Map[String, typed.FunctionType]]()


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
          c.name.name -> typed.FunctionType(c.params.map(t => checkType(t.typ)(typeContext)), SimpleType(name, t.typeParameters.map(v => typed.TypeVarUse(v.name.name)()))(), FunctionKindDatatypeConstructor())()
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
        expectedReturn = UnitType()
      )
      nameBindings = nameBindings ++ crdtunfold(nameBindings, crdt.keyDecl)
    }

    for (p <- program.procedures) {
      val paramTypes: List[InTypeExpr] = p.params.map(_.typ)
      // invocation info constructor
      nameBindings += (p.name.name -> typed.FunctionType(paramTypes.map(checkType(_)(typeContext)), InvocationInfoType(), FunctionKindDatatypeConstructor())())
      // invocation result constructor
      val returnTypeL = checkType(p.returnType)(typeContext) match {
        case UnitType() => List()
        case rt => List(rt)
      }
      nameBindings += (s"${p.name.name}_res" -> typed.FunctionType(returnTypeL, InvocationResultType(), FunctionKindDatatypeConstructor())())
    }

    val preContext = Context(
      types = nameBindings,
      declaredTypes = declaredTypes,
      datatypes = datatypes,
    )

    implicit val baseContext: Context = preContext.copy(
      types = nameBindings,
      datatypes = datatypes
    )

    var crdts = Map[String, ACrdtInstance]()
    for (crdt <- program.crdts) {
      toInstance(crdt.keyDecl.crdttype) match {
        case Left(instance) =>
          crdts += crdt.keyDecl.name.name -> instance
        case Right(_) =>
      }
    }


    val checkedProgram = typed.InProgram(
      name = program.name,
      source = program.source,
      procedures = program.procedures.map(checkProcedure),
      types = program.types.map(checkTypeDecl),
      axioms = program.axioms.map(checkAxiom),
      invariants = program.invariants.zipWithIndex.map(checkInvariant),
      programCrdt = StructInstance(fields = crdts)
    )

    if (errors.isEmpty) {
      NormalResult(checkedProgram)
    } else {
      ErrorResult(errors)
    }
  }


  def checkProcedure(p: InProcedure)(implicit ctxt: Context): typed.InProcedure = {
    val vars: List[InVariable] = p.params ++ p.locals

    val cs = TypeConstraints()
    val (argTypes, cs2) = getArgTypesC(vars, cs)
    val (returnType, cs3) = checkTypeC(p.returnType, cs2)


    val typesWithParams = ctxt.types ++ argTypes
    val newCtxt = ctxt.copy(
      types = typesWithParams,
      expectedReturn = returnType
    )

    val (body, cs4) = checkStatement(p.body, cs3)(newCtxt)

    typed.InProcedure(
      name = p.name,
      source = p.source,
      body = checkStatement(p.body)(newCtxt),
      params = checkParams(p.params),
      locals = checkParams(p.locals),
      returnType = returnType
    )
  }

  def getArgTypesT(vars: List[typed.InVariable]): List[(String, typed.InTypeExpr)] = {
    for (param <- vars) yield param.name.name -> param.typ
  }

  def getArgTypes(vars: List[InVariable])(implicit ctxt: Context): List[(String, PrincipleType)] = {
    for (param <- vars) yield
      param.name.name -> PrincipleType(List(), checkType(param.typ))
  }

  def getArgTypesC(vars: List[InVariable])(implicit ctxt: Context): TypeResult[List[(String, PrincipleType)]] = {
    vars.mapM { param =>
      for {
        t <- checkTypeC(param.typ)
      } yield
        param.name.name -> PrincipleType(List(), t)
    }
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

  def checkVariableC(variable: InVariable)(implicit ctxt: Context): TypeResult[typed.InVariable] = {
    for {
      t <- checkTypeC(variable.typ)
    } yield typed.InVariable(name = variable.name, source = variable.source, typ = t)
  }


  def checkParams(params: List[InVariable])(implicit ctxt: Context): List[typed.InVariable] = {
    params.map(checkVariable)
  }

  def checkParamsC(params: List[InVariable])(implicit ctxt: Context): TypeResult[List[typed.InVariable]] = {
    params.mapM(checkVariableC)
  }

  def checkType(t: InTypeExpr)(implicit ctxt: TypeContext): typed.InTypeExpr = t match {
    case UnresolvedType("Bool", List()) =>
      typed.BoolType()
    case UnresolvedType(name, typeArgs) =>
      lookupType(t.getSource(), name, typeArgs.map(checkType))
    case f: FunctionType =>
      checkFunctionType(f, ???)
    case _: InferType =>
      addError(t, "Missing type annotation.")
      typed.AnyType()
  }

  /** like checkType but uses type constraints */
  def checkTypeC(t: InTypeExpr)(implicit ctxt: TypeContext): TypeResult[typed.InTypeExpr] = t match {
    case _: InferType =>
      State(_.freshVar)
    case _ =>
      State.pure(checkType(t))
  }

  def lookupType(source: SourceTrace, name: String, typeArgs: List[TypedAst.InTypeExpr])(implicit ctxt: TypeContext): TypedAst.InTypeExpr = {
    ctxt.declaredTypes.get(name) match {
      case None =>
        addError(source, s"Could not find type $name.")
        typed.AnyType()
      case Some(f) =>
        if (f.arity != typeArgs.length)
          addError(source, s"Type $name requires ${f.arity} type arguments but ${typeArgs.length} arguments are given.")
        f.makeType(typeArgs)
    }
  }

  def checkFunctionType(f: FunctionType, kind: FunctionKind)(implicit ctxt: TypeContext): typed.FunctionType = {
    typed.FunctionType(f.argTypes.map(checkType), checkType(f.returnType), kind)(source = f.getSource())
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

  def checkInvariant(ii: (InInvariantDecl, Int))(implicit ctxt: Context): typed.InInvariantDecl = {
    val i = ii._1
    typed.InInvariantDecl(
      source = i.source,
      name = if (i.name.isEmpty) s"inv${ii._2}" else i.name,
      isFree = i.isFree,
      expr = checkExpr(i.expr)
    )
  }


  def lookup(varname: Identifier)(implicit ctxt: Context): typed.InTypeExpr = {
    ctxt.types.getOrElse[typed.InTypeExpr](varname.name, {
      val suggestions = ctxt.types.keys.toList.sorted
      addError(varname, s"Could not find declaration of ${varname.name}.\nAvailable names: ${suggestions.mkString(", ")}")
      AnyType()
    })
  }


  def checkStatements(stmts: List[InStatement])(implicit ctxt: Context): TypeResult[List[LazyBound[typed.InStatement]]] = {
    stmts.mapM(checkStatement)
  }

  def addConstraint(c: TypeConstraint): TypeResult[Unit] = {
    State.modify(_.withConstraint(c))
  }

  def checkStatement(s: InStatement)(implicit ctxt: Context): TypeResult[LazyBound[typed.InStatement]] = s match {
    case BlockStmt(source, stmts) =>
      for {
        stmtsT <- checkStatements(stmts)
      } yield LazyBound { subst =>
        typed.BlockStmt(source, stmtsT.map(_.bind(subst)))
      }
    case Atomic(source, body) =>
      for {
        s <- checkStatement(body)
      } yield LazyBound { subst =>
        typed.Atomic(source, s.bind(subst))
      }
    case LocalVar(source, variable) =>
      for {
        v <- checkVariableC(variable)
      } yield
        typed.LocalVar(source, v)
    case IfStmt(source, cond, thenStmt, elseStmt) =>
      for {
        c <- checkExpr(cond)
        _ <- addConstraint(TypesEqual(c.typ, BoolType())(cond.getSource()))
        t <- checkStatement(thenStmt)
        e <- checkStatement(elseStmt)
      } yield LazyBound { subst =>
        typed.IfStmt(source, c.bind(subst), t.bind(subst), e.bind(subst))
      }
    //
    //      val condTyped: typed.InExpr = checkExpr(cond)
    //      if (!condTyped.getTyp.isSubtypeOf(BoolType())) {
    //        addError(cond, s"Expression of if-statement must be boolean, but was ${condTyped.getTyp}.")
    //      }
    //      typed.IfStmt(source, condTyped, checkStatement(thenStmt), checkStatement(elseStmt))
    case MatchStmt(source, expr, cases) =>
      // TODO add exhaustiveness and distinctiveness test
      for {
        exprTyped <- checkExpr(expr)
        casesTyped <- cases.mapM(checkCase(_, exprTyped.typ))
      } yield LazyBound { subst =>
        typed.MatchStmt(source, exprTyped.bind(subst), casesTyped.bind(subst))
      }
    case CrdtCall(source, call) =>
      val callTyped = checkFunctionCall(call)
      if (!callTyped.getTyp.isSubtypeOf(SomeOperationType())) {
        addError(call, s"Not an operation.")
      }
      typed.CrdtCall(source, callTyped)
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

      t match {
        case it: IdType =>
          if (!t.isSubtypeOf(varType)) {
            addError(typename, s"Cannot assign id $t to variable of type $varType.")
          }

          typed.NewIdStmt(source, varname, it)
        case _ =>
          addError(typename, s"Type $t must be declared as idType.")
          typed.makeBlock(source, List())
      }
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


  def checkCase(c: MatchCase, expectedType: typed.InTypeExpr)(implicit ctxt: Context): TypeResult[LazyBound[typed.MatchCase]] = {
    for {
      e <- checkPattern(c.pattern, expectedType)(ctxt)
      (newCtxt, patternTyped) = e
      stmtTyped <- checkStatement(c.statement)(newCtxt)
    } yield LazyBound { subst =>
      typed.MatchCase(
        source = c.source,
        pattern = patternTyped.bind(subst),
        statement = stmtTyped.bind(subst)
      )
    }
  }

  def checkPattern(pattern: InExpr, expectedType: typed.InTypeExpr)(implicit ctxt: Context): TypeResult[(Context, LazyBound[typed.InExpr])] = pattern match {
    case VarUse(source, name) =>
      if (ctxt.types.contains(name)) {
        addError(pattern, s"Variable with name $name is already defined.")
      }
      val newCtxt = ctxt.withBinding(name, expectedType)
      State.pure((newCtxt, typed.VarUse(source, expectedType, name)))
    case f@FunctionCall(source, functionName, args) =>
      ctxt.types.get(functionName.name) match {
        case Some(PrincipleType(typeParams, FunctionType(argTypes, returnType, FunctionKindDatatypeConstructor))) =>
          // check if args have same length

          // create new variables for typeParams

          // substitute new type params in argTypes and returnType

          // add new constraints: expected type must be equal to returnType

          // match args patterns recursively

        case Some(_) =>
          addError(f, s"Function $functionName is not a datatype constructor.")
          AnyType()
        case None =>
          addError(f, s"Could not find pattern $functionName.")
      }

      val matchingTypes: List[(String, TypedAst.FunctionType)] =
        (for {
          (dtName, dt) <- ctxt.datatypes
          (caseName, caseTyp) <- dt
          if caseName == functionName.name
        } yield (dtName, caseTyp)).toList
      var newCtxt = ctxt


      expectedType match {
        case SimpleType(dtName, typeArgs) =>
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
      (ctxt, typed.VarUse(f.getSource(), AnyType(), "_"))
  }

  def checkExpr(e: InExpr)(implicit ctxt: Context): TypeResult[ExprResult] = e match {
    case v@VarUse(source, name) =>
      val t = ctxt.types.getOrElse[typed.InTypeExpr](name, {
        addError(e, s"Could not find declaration of $name.")
        AnyType()
      })
      typed.VarUse(source, t, name)
    case BoolConst(source, v) =>
      typed.BoolConst(source, BoolType(), v)
    case IntConst(source, value) =>
      typed.IntConst(source, IntType(), value)
    case fc: FunctionCall =>
      checkFunctionCall(fc)
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
        case BF_distinct() =>
          typedArgs.map(_ => typedArgs.head.getTyp) -> BoolType()
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
          val t1 = typedArgs(0).getTyp
          if (t1.isSubtypeOf(CallIdType()))
            List(CallIdType()) -> InvocationIdType()
          else if (t1.isSubtypeOf(TransactionIdType()))
            List(TransactionIdType()) -> InvocationIdType()
          else {
            addError(e, s"Origin is only available for calls and transactions.")
            List(AnyType()) -> InvocationIdType()
          }
        case BF_getTransaction() =>
          List(CallIdType()) -> TransactionIdType()
        case BF_inCurrentInvoc() =>
          List(CallIdType()) -> BoolType()
      }
      val (argTypes, t) = tuple
      checkCall(ab, typedArgs, argTypes)
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


  def checkCall(source: AstElem, typedArgs: List[typed.InExpr], argTypes: List[typed.InTypeExpr]): Unit = {
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

  def checkFunctionCall(fc: FunctionCall)(implicit ctxt: Context): typed.FunctionCall = {
    val typedArgs = fc.args.map(checkExpr)
    val (newKind, t) = lookup(fc.functionName) match {
      case typed.FunctionType(argTypes, returnType, kind) =>
        checkCall(fc, typedArgs, argTypes)
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

  sealed abstract class TypeConstraint {
    def trace: SourceTrace
  }

  case class TypesEqual(left: typed.InTypeExpr, right: typed.InTypeExpr)(val trace: SourceTrace) extends TypeConstraint

  case class Alternative(left: List[TypeConstraint], right: List[TypeConstraint])(val trace: SourceTrace) extends TypeConstraint


  case class TypeConstraints(
    constraints: List[TypeConstraint] = List(),
    varCount: Int = 0
  ) {
    def withConstraint(c: TypeConstraint): TypeConstraints = {
      copy(constraints = c :: constraints)
    }


    def freshVar: (TypeVarUse, TypeConstraints) =
      (TypeVarUse(s"X$varCount")(), copy(varCount = varCount + 1))


    def solve: Either[TypeErrorException, Map[TypeVarUse, typed.InTypeExpr]] = {

      /** put alternatives at the end to avoid extensive backtracking */
      def tcSort(list: List[TypeConstraint]): List[TypeConstraint] = {
        val (normalConstraints, alternatives) = list.partition(!_.isInstanceOf[Alternative])
        normalConstraints ++ alternatives
      }

      def unify(list: List[TypeConstraint], subst: Map[TypeVarUse, typed.InTypeExpr]): Either[TypeErrorException, Map[TypeVarUse, typed.InTypeExpr]] =
        list match {
          case List() => Right(subst)
          case c :: cs =>
            c match {
              case tec@TypesEqual(left, right) =>
                (left, right) match {
                  case _ if left == right =>
                    unify(cs, subst)
                  case (_: AnyType, _) | (_, _: AnyType) =>
                    // ignore any-constraints
                    unify(cs, subst)
                  case (v: TypeVarUse, r) =>
                    if (r.freeVars.contains(v))
                      Left(new TypeErrorException(tec.trace, s"Cannot match type $v with $r (failed occurs check)"))
                    else
                      subst.get(v) match {
                        case Some(l) =>
                          // apply subst
                          unify(TypesEqual(l, r) :: cs, subst)
                        case None =>
                          // add to subst
                          unify(cs, subst + (v -> r))
                      }
                  case (t, v: TypeVarUse) =>
                    // swap
                    unify(TypesEqual(v, t) :: cs, subst)
                  case (typed.FunctionType(as1, r1, _), typed.FunctionType(as2, r2, _)) if as1.length == as2.length =>
                    // unify type args:
                    unify(as1.zip(as2).map(e => TypesEqual(e._1, e._2)(tec.trace)) ::
                      TypesEqual(r1, r2)(tec.trace) ::
                      cs, subst)
                  case (SimpleType(t1, as1), SimpleType(t2, as2)) if t1 == t2 && as1.length == as2.length =>
                    // unify type args:
                    unify(as1.zip(as2).map(e => TypesEqual(e._1, e._2)(tec.trace)) ::
                      cs, subst)
                  case _ =>
                    Left(new TypeErrorException(tec.trace, s"Could not match types $left and $right."))
                }


              case Alternative(left, right) =>
                unify(left ++ cs, subst)
                  .orElse(unify(right ++ cs, subst))
            }

        }

      unify(tcSort(constraints), Map())
    }
  }


}


