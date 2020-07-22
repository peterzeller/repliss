package crdtver.language

import crdtver.Repliss
import crdtver.Repliss._
import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst._
import crdtver.language.TypedAst.FunctionKind.{FunctionKindCrdtQuery, FunctionKindDatatypeConstructor}
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionKind, IdType, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, PrincipleType, SimpleType, SomeOperationType, Subst, TransactionIdType, TypeVarUse, UnitType}
import crdtver.language.Typer.{Alternative, TypeConstraint, TypeConstraints, TypeErrorException, TypesEqual}
import crdtver.language.crdts.{ACrdtInstance, CrdtTypeDefinition, StructCrdt}
import crdtver.language.{TypedAst => typed}
import crdtver.utils.ListExtensions
import ListExtensions.ListUtils
import cats.{Eval, Functor, Monad}
import cats.data.{IndexedStateT, State}
import cats.implicits._
import crdtver.language.crdts.ACrdtInstance.QueryStructure
import info.debatty.java.stringsimilarity.JaroWinkler
import info.debatty.java.stringsimilarity.interfaces.{StringDistance, StringSimilarity}

import scala.annotation.tailrec
import scala.language.implicitConversions

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


    def withTypeBinding(tpName: String, t: typed.InTypeExpr): TypeContext
  }

  final case class TypeContextImpl(declaredTypes: Map[String, TypeFactory]) extends TypeContext {
    def withTypeBinding(tpName: String, t: typed.InTypeExpr): TypeContextImpl =
      copy(declaredTypes + (tpName -> TypeFactory(0, _ => t)))

  }


  final case class Context(
    // types of variables
    types: Map[String, PrincipleType],
    declaredTypes: Map[String, TypeFactory],
    datatypes: Map[String, Map[String, typed.PrincipleType]],
    // expected return value of current procedure
    expectedReturn: typed.InTypeExpr = UnitType(),
    // type of toplevel CRDT operation
    programCrdt: Option[ACrdtInstance] = None

  ) extends TypeContext {
    def withBinding(varname: String, typ: typed.PrincipleType): Context = {
      copy(
        types = types + (varname -> typ)
      )
    }

    def withBinding(varname: String, typ: typed.InTypeExpr): Context = {
      withBinding(varname, PrincipleType(List(), typ))
    }

    def withTypeBinding(tpName: String, t: typed.InTypeExpr): Context =
      copy(declaredTypes = declaredTypes + (tpName -> TypeFactory(0, _ => t)))

    def operationType: Option[TypedAst.InTypeExpr] =
      programCrdt.map(_.operationType)

    def queryType: Option[TypedAst.InTypeExpr] =
      programCrdt.map(_.queryType)
  }

  /** can only construct T after type constraint solution exists */
  case class LazyBound[+T](
    bind: Subst => T
  )

  case class ExprResult(
    typ: typed.InTypeExpr,
    source: SourceTrace,
    makeExpr: LazyBound[typed.InExpr]
  ) {
    def bind(subst: Subst): TypedAst.InExpr =
      makeExpr.bind(subst)
  }

  //  type TypeResult[+T] = State[TypeConstraints, T]
  //
  case class TypeResult[+T](
    step: TypeConstraints => (T, TypeConstraints)
  ) {
    def run(constraints: TypeConstraints): (TypeConstraints, T) = step(constraints).swap

  }

  object TypeResult {
    def modify(f: TypeConstraints => TypeConstraints): TypeResult[Unit] =
      TypeResult(s => ((), f(s)))

    def pure[T](x: T): TypeResult[T] = TypeResult(s => (x, s))
  }

  implicit def typeResultFunctor[T]: Monad[TypeResult] = new Monad[TypeResult] {
    override def map[A, B](fa: TypeResult[A])(f: A => B): TypeResult[B] =
      TypeResult(s => {
        val (x, s2) = fa.step(s)
        (f(x), s2)
      })

    override def pure[A](x: A): TypeResult[A] =
      TypeResult(s => (x, s))

    override def flatMap[A, B](fa: TypeResult[A])(f: A => TypeResult[B]): TypeResult[B] = {
      TypeResult(s => {
        val (x, s2) = fa.step(s)
        f(x).step(s2)
      })

    }

    override def tailRecM[A, B](a: A)(f: A => TypeResult[Either[A, B]]): TypeResult[B] = {
      TypeResult { initialState =>
        @scala.annotation.tailrec
        def rec(currentState: TypeConstraints, currentVal: A): (B, TypeConstraints) = {
          val (r, newState) = f(currentVal).step(currentState)
          r match {
            case Left(newVal) =>
              rec(newState, newVal)
            case Right(r) =>
              (r, newState)
          }
        }

        rec(initialState, a)
      }
    }
  }


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
            return Left(crdt.instantiate(typeArgs, crdtArgs))
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
        Left(new StructCrdt(map.keys.mkString("|"), map).instantiate())
    }
  }

  def crdtunfold(nameBindings: Map[String, PrincipleType], key: InKeyDecl)(implicit ctxt: Context): Map[String, PrincipleType] = {
    var tempBindings = nameBindings
    // TODO refactor CRDTs
    //    toInstance(key.crdttype) match {
    //      case Left(a) =>
    //        for (op <- a.operations()) {
    //          val opName = key.name.name + '_' + op.name
    //          if (tempBindings.contains(opName)) {
    //            addError(key.crdttype, s"Element with name $opName already exists.")
    //          }
    //          tempBindings += (opName -> typed.FunctionType(op.paramTypes, typed.OperationType(opName)(), FunctionKindDatatypeConstructor())())
    //        }
    //        for (q <- a.queries()) {
    //          val qName = key.name.name + '_' + q.qname
    //          if (tempBindings.contains(qName)) {
    //            addError(key.crdttype, s"Element with name $qName already exists.")
    //          }
    //          tempBindings += (qName -> typed.FunctionType(q.qparamTypes, q.qreturnType, FunctionKindCrdtQuery())())
    //          val queryDtName = s"queryop_$qName"
    //          val queryDtArgs: List[TypedAst.InTypeExpr] = q.qparamTypes :+ q.qreturnType
    //          tempBindings += (queryDtName -> typed.FunctionType(queryDtArgs, typed.OperationType(queryDtName)(), FunctionKindDatatypeConstructor())())
    //        }
    //
    //      case Right(b) =>
    //        addError(key.crdttype, "Invalid type: " + b)
    //    }
    tempBindings
  }

  def addError(elem: AstElem, msg: String): Unit = {
    addError(elem.getSource(), msg)
  }

  private def addError(source: SourceTrace, msg: String): Unit = {
    val err = Error(source.range, msg)
    errors :+= err
    //throw new TypeErrorException(source, msg)
  }

  def addError(elem: typed.AstElem, msg: String): Unit = {
    addError(elem.getSource(), msg)
  }

  def addTypeParameters[T <: TypeContext](typeContext: T, typeParameters: List[TypeParameter]): T = {
    var localTypeContext: T = typeContext
    for (tp <- typeParameters) {
      val tpName = tp.name.name
      if (typeContext.declaredTypes.isDefinedAt(tpName)) {
        addError(tp.getSource(), s"Type $tpName is already defined.")
      } else {
        localTypeContext = localTypeContext.withTypeBinding(tpName, typed.TypeVarUse(tpName)(tp.source)).asInstanceOf[T]
      }
    }
    localTypeContext
  }

  def checkProgram(program: InProgram): Result[typed.InProgram] = {
    var nameBindings = Map[String, PrincipleType](
      "NoResult" -> PrincipleType(List(), typed.FunctionType(List(), typed.InvocationResultType(), FunctionKindDatatypeConstructor())())
    )
    var declaredTypes = Map[String, TypeFactory](
      "Bool" -> noArgsType(typed.BoolType()),
      "Int" -> noArgsType(typed.IntType()),
      "CallId" -> noArgsType(typed.CallIdType()),
      "InvocationId" -> noArgsType(typed.InvocationIdType()),
      "TransactionId" -> noArgsType(typed.TransactionIdType()),
      "InvocationInfo" -> noArgsType(typed.InvocationInfoType())
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


    var datatypes = Map[String, Map[String, typed.PrincipleType]]()


    val typeContext: TypeContext = TypeContextImpl(declaredTypes)


    // build toplevel context:
    for (query <- program.queries) {
      val name = query.name.name
      if (nameBindings contains name) {
        addError(query, s"Element with name $name already exists.")
      }
      nameBindings += (name -> typed.PrincipleType(List(),
        typed.FunctionType(query.params.map(t => checkType(t.typ)(typeContext)), checkType(query.returnType)(typeContext), FunctionKindCrdtQuery())()))
    }

    for (operation <- program.operations) {
      val name = operation.name.name
      nameBindings += (name -> typed.PrincipleType(List(),
        typed.FunctionType(operation.params.map(t => checkType(t.typ)(typeContext)), OperationType(name)(), FunctionKindDatatypeConstructor())()))
    }

    def addTypeToContext(t: InTypeDecl): Unit = {
      val name = t.name.name
      implicit val localTypeContext: TypeContext = addTypeParameters(typeContext, t.typeParameters)
      val typeParams: List[TypeVarUse] =
        for (tp <- t.typeParameters) yield {
          typed.TypeVarUse(tp.name.name)()
        }
      val dtCases: List[(String, typed.PrincipleType)] =
        for (c <- t.dataTypeCases) yield {
          val paramTypes = c.params.map(t => checkType(t.typ)(localTypeContext))
          val returnType = SimpleType(name, typeParams)()
          c.name.name -> typed.PrincipleType(typeParams, typed.FunctionType(paramTypes, returnType, FunctionKindDatatypeConstructor())())
        }
      if (dtCases.nonEmpty) {
        nameBindings = nameBindings ++ dtCases
        datatypes += name -> (Map() ++ dtCases)
      }

      val dtType = typed.SimpleType(name, typeParams)()
      // add selector functions
      val selectorFunctions: List[(String, PrincipleType)] =
        for (c <- t.dataTypeCases; p <- c.params)
          yield p.name.name -> PrincipleType(typeParams, typed.FunctionType(List(dtType), checkType(p.typ)(localTypeContext), FunctionKindDatatypeConstructor())())

      nameBindings = nameBindings ++ selectorFunctions.toMap
    }


    for (t <- program.types) {
      addTypeToContext(t)
    }


    //    for (crdt <- program.crdts) {
    //      implicit val typeCtxt: Context = Context(
    //        types = nameBindings,
    //        declaredTypes = declaredTypes,
    //        datatypes = datatypes,
    //        expectedReturn = UnitType()
    //      )
    //      nameBindings = nameBindings ++ crdtunfold(nameBindings, crdt.keyDecl)(typeCtxt)
    //    }
    val preContext1 = Context(
      types = nameBindings,
      declaredTypes = declaredTypes,
      datatypes = datatypes,
    )


    var crdts = Map[String, ACrdtInstance]()
    for (crdt <- program.crdts) {


      toInstance(crdt.keyDecl.crdttype)(preContext1) match {
        case Left(instance) =>
          crdts += crdt.keyDecl.name.name -> instance
        case Right(_) =>
      }
    }

    val programCrdt: ACrdtInstance = new StructCrdt("rootCrdt", fields = crdts).instantiate()


    def addTypeToContextT(t: TypedAst.InTypeDecl): Unit = {
      val name = t.name.name
      val typeParams: List[TypeVarUse] =
        for (tp <- t.typeParameters) yield {
          typed.TypeVarUse(tp.name.name)()
        }
      val dtCases: List[(String, typed.PrincipleType)] =
        for (c <- t.dataTypeCases) yield {
          val paramTypes = c.params.map(t => t.typ)
          val returnType = SimpleType(name, typeParams)()
          c.name.name -> typed.PrincipleType(typeParams, typed.FunctionType(paramTypes, returnType, FunctionKindDatatypeConstructor())())
        }
      if (dtCases.nonEmpty) {
        nameBindings = nameBindings ++ dtCases
        datatypes += name -> (Map() ++ dtCases)
      }

      val dtType = typed.SimpleType(name, typeParams)()
      // add selector functions
      val selectorFunctions: List[(String, PrincipleType)] =
        for (c <- t.dataTypeCases; p <- c.params)
          yield p.name.name -> PrincipleType(typeParams, typed.FunctionType(List(dtType), p.typ, FunctionKindDatatypeConstructor())())

      nameBindings = nameBindings ++ selectorFunctions.toMap
    }

    val crdtAdditionalDataTypes: List[TypedAst.InTypeDecl] = programCrdt.additionalDataTypesRec.distinct
    for (t <- crdtAdditionalDataTypes) {
      addTypeToContextT(t)
    }


    for (p <- program.procedures) {
      val paramTypes: List[InTypeExpr] = p.params.map(_.typ)
      // invocation info constructor
      nameBindings += (p.name.name -> typed.PrincipleType(List(),
        typed.FunctionType(paramTypes.map(checkType(_)(typeContext)), InvocationInfoType(), FunctionKindDatatypeConstructor())()))
      // invocation result constructor
      val returnTypeL = checkType(p.returnType)(typeContext) match {
        case UnitType() => List()
        case rt => List(rt)
      }
      nameBindings += (s"${p.name.name}_res" -> typed.PrincipleType(List(),
        typed.FunctionType(returnTypeL, InvocationResultType(), FunctionKindDatatypeConstructor())()))
    }


    implicit val baseContext: Context = Context(
      types = nameBindings,
      declaredTypes = declaredTypes,
      datatypes = datatypes,
      programCrdt = Some(programCrdt)
    )


    val checkedProgram = typed.InProgram(
      name = program.name,
      source = program.source,
      procedures = program.procedures.map(checkProcedure),
      types = program.types.map(checkTypeDecl) ++ crdtAdditionalDataTypes,
      axioms = program.axioms.map(checkAxiom),
      invariants = program.invariants.zipWithIndex.map(checkInvariant),
      programCrdt = programCrdt
    )

    if (errors.isEmpty) {
      NormalResult(checkedProgram)
    } else {
      ErrorResult(errors)
    }
  }


  def checkProcedure(p: InProcedure)(implicit ctxt: Context): typed.InProcedure = checkToplevelLazyBound {
    val vars: List[InVariable] = p.params ++ p.locals

    for {
      argTypes <- getArgTypesC(vars)
      //      returnType <- checkTypeC(p.returnType, s"${p.name}_return")
      returnType = checkType(p.returnType)
      typesWithParams = ctxt.types ++ argTypes
      newCtxt = ctxt.copy(
        types = typesWithParams,
        expectedReturn = returnType
      )
      body <- checkStatement(p.body)(newCtxt)
    } yield LazyBound { subst =>
      typed.InProcedure(
        name = p.name,
        source = p.source,
        body = body.bind(subst),
        params = checkParams(p.params).map(_.subst(subst)),
        locals = checkParams(p.locals).map(_.subst(subst)),
        returnType = returnType.subst(subst)
      )
    }
  }

  def getArgTypesT(vars: List[typed.InVariable]): List[(String, typed.PrincipleType)] = {
    for (param <- vars) yield param.name.name -> PrincipleType(List(), param.typ)
  }

  def getArgTypes(vars: List[InVariable])(implicit ctxt: Context): List[(String, PrincipleType)] = {
    for (param <- vars) yield
      param.name.name -> PrincipleType(List(), checkType(param.typ))
  }

  def getArgTypesC(vars: List[InVariable])(implicit ctxt: Context): TypeResult[List[(String, PrincipleType)]] = {
    vars.traverse { param =>
      for {
        t <- checkTypeC(param.typ, s"${param.name}T")
      } yield
        param.name.name -> PrincipleType(List(), t)
    }
  }

  private def transformTypeParams(ts: List[TypeParameter]): List[TypedAst.TypeParameter] =
    ts.map(transformTypeParam)

  private def transformTypeParam(tp: TypeParameter): TypedAst.TypeParameter =
    TypedAst.TypeParameter(tp.source, tp.name)


  def checkTypeDecl(t: InTypeDecl)(implicit ctxt: Context): typed.InTypeDecl = {
    val ctxt2 = addTypeParameters(ctxt, t.typeParameters)
    // TODO checks necessary?
    typed.InTypeDecl(
      name = t.name,
      source = t.source,
      isIdType = t.isIdType,
      typeParameters = transformTypeParams(t.typeParameters),
      dataTypeCases = t.dataTypeCases.map(c => typed.DataTypeCase(
        name = c.name,
        source = c.source,
        params = checkParams(c.params)(ctxt2)
      ))
    )
  }

  def checkVariable(variable: InVariable)(implicit ctxt: Context): typed.InVariable =
    typed.InVariable(name = variable.name, source = variable.source, typ = checkType(variable.typ))

  def checkVariableC(variable: InVariable)(implicit ctxt: Context): TypeResult[typed.InVariable] = {
    for {
      t <- checkTypeC(variable.typ, s"${variable.name}T")
    } yield typed.InVariable(name = variable.name, source = variable.source, typ = t)
  }


  def checkParams(params: List[InVariable])(implicit ctxt: Context): List[typed.InVariable] = {
    params.map(checkVariable)
  }

  def checkParamsC(params: List[InVariable])(implicit ctxt: Context): TypeResult[List[typed.InVariable]] = {
    params.traverse(checkVariableC)
  }

  def checkType(t: InTypeExpr)(implicit ctxt: TypeContext): typed.InTypeExpr = t match {
    case UnresolvedType("Bool", List()) =>
      typed.BoolType()
    case UnresolvedType(name, typeArgs) =>
      lookupType(t.getSource(), name, typeArgs.map(checkType))
    case f: FunctionType =>
      checkFunctionType(f, ???)
    case _: InferType =>
      typed.UnitType()
  }

  def freshVar(name: String = "X"): TypeResult[typed.TypeVarUse] =
    TypeResult(_.freshVar(name))

  /** like checkType but uses type constraints */
  def checkTypeC(t: InTypeExpr, nameHint: String)(implicit ctxt: TypeContext): TypeResult[typed.InTypeExpr] = t match {
    case _: InferType =>
      freshVar(nameHint)
    case _ =>
      TypeResult.pure(checkType(t))
  }

  def lookupType(source: SourceTrace, name: String, typeArgs: List[TypedAst.InTypeExpr])(implicit ctxt: TypeContext): TypedAst.InTypeExpr = {
    if (name == "Unit")
      return typed.UnitType()
    ctxt.declaredTypes.get(name) match {
      case None =>
        val suggestions = ctxt.declaredTypes.keys.toList.sorted
        addError(source, s"Could not find type $name.\nAvailable types: ${suggestions}")
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

  def checkQuery(q: InQueryDecl)(implicit ctxt: Context): typed.InQueryDecl = checkToplevelLazyBound {
    lazy val newCtxt = ctxt.copy(
      types = ctxt.types ++ getArgTypes(q.params)
    )


    for {
      returnType <- checkTypeC(q.returnType, s"${q.name}_return")
      ensuresCtxt = newCtxt.copy(
        types = newCtxt.types + ("result" -> typed.PrincipleType(List(), returnType))
      )
      implTyped <- q.implementation.traverse(checkExpr(_)(newCtxt))
      _ <- implTyped.traverse(e => addConstraint(TypesEqual(e.typ, returnType)(q.getSource(), (a, e) => s"Implementation of type $a does not match query return type $e.")))
      ensuresTyped <- q.ensures.traverse(checkExpr(_)(ensuresCtxt))
      _ <- ensuresTyped.traverse(e => addConstraint(TypesEqual(e.typ, BoolType())(q.getSource(), (a, e) => s"Ensures clause must be of type Bool, but found $a.")))
    } yield LazyBound { s =>
      typed.InQueryDecl(
        name = q.name,
        source = q.source,
        annotations = q.annotations,
        implementation = implTyped.map(_.bind(s)),
        ensures = ensuresTyped.map(_.bind(s)),
        params = checkParams(q.params),
        returnType = returnType
      )
    }


  }

  def checkToplevelLazyBound[T](tr: TypeResult[LazyBound[T]]): T = {
    val (constraints, t) = tr.run(TypeConstraints())
    val sol = constraints.solve
    for (err <- sol.errors) {
      addError(err.source, err.message)
    }
    t.bind(sol.subst)
  }

  def checkAxiom(p: InAxiomDecl)(implicit ctxt: Context): typed.InAxiomDecl = checkToplevelLazyBound {
    for {
      e <- checkExpr(p.expr)
      _ <- addConstraint(TypesEqual(e.typ, BoolType())(p.expr.getSource(), (a, e) => s"Axiom must be of type Bool but found $a."))
    } yield LazyBound { s =>
      typed.InAxiomDecl(
        source = p.source,
        expr = e.bind(s)
      )
    }
  }


  def checkInvariant(ii: (InInvariantDecl, Int))(implicit ctxt: Context): typed.InInvariantDecl = checkToplevelLazyBound {
    val i = ii._1
    for {
      e <- checkExpr(i.expr)
      _ <- addConstraint(TypesEqual(e.typ, BoolType())(i.expr.getSource(), (a, e) => s"Invariant must be of type Bool but found $a."))
    } yield LazyBound { s =>
      typed.InInvariantDecl(
        source = i.source,
        name = if (i.name.isEmpty) s"inv${ii._2}" else i.name,
        isFree = i.isFree,
        expr = e.bind(s)
      )
    }
  }


  def distanceTo(name: String)(other: String): Double = {
    val s: StringDistance = new JaroWinkler()
    s.distance(name, other)
  }

  def lookup(varname: Identifier)(implicit ctxt: Context): typed.PrincipleType = {
    ctxt.types.getOrElse[typed.PrincipleType](varname.name, {
      val suggestions = ctxt.types.keys.toList.sortBy(distanceTo(varname.name)).take(3)
      addError(varname, s"Could not find declaration of ${varname.name}.\nSimilar names: ${suggestions.mkString(", ")}")
      PrincipleType(List(), AnyType())
    })
  }

  def lookupVar(varname: Identifier)(implicit ctxt: Context): typed.InTypeExpr = {
    val t = lookup(varname)
    if (t.typeParams.nonEmpty)
      addError(varname, s"Variable ${varname.name} does not have a concrete type.")
    t.typ
  }


  def checkStatements(stmts: List[InStatement])(implicit ctxt: Context): TypeResult[List[LazyBound[typed.InStatement]]] = {
    stmts.traverse(checkStatement)
  }

  def addConstraint(c: TypeConstraint): TypeResult[Unit] = {
    TypeResult.modify(_.withConstraint(c))
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
      } yield LazyBound { s =>
        typed.LocalVar(source, v.subst(s))
      }
    case IfStmt(source, cond, thenStmt, elseStmt) =>
      for {
        c <- checkExpr(cond)
        _ <- addConstraint(TypesEqual(c.typ, BoolType())(cond.getSource(), (a, e) => s"Condition must be of type Bool but found $a."))
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
        casesTyped <- cases.traverse(checkCase(_, exprTyped.typ))
      } yield LazyBound { subst =>
        typed.MatchStmt(source, exprTyped.bind(subst), casesTyped.map(_.bind(subst)))
      }
    case CrdtCall(source, call) =>
      for {
        callTyped <- checkFunctionCall(call)
        _ <- addConstraint(TypesEqual(callTyped.typ, ctxt.operationType.get)(source, (a, e) => s"Type $a is not an operation."))
      } yield LazyBound { s =>
        typed.CrdtCall(source, callTyped.bind(s).asInstanceOf[typed.FunctionCall])
      }
    case Assignment(source, varname, expr) =>
      val varType: typed.InTypeExpr = lookupVar(varname)
      for {
        exprTyped <- checkExpr(expr)
        _ <- addConstraint(TypesEqual(exprTyped.typ, varType)(expr.getSource(), (a, e) => s"Cannot assign expression of type $a to variable $varname of type $e."))
      } yield LazyBound { s =>
        typed.Assignment(source, varname, exprTyped.bind(s))
      }
    case NewIdStmt(source, varname, typename) =>
      val varType: typed.InTypeExpr = lookupVar(varname)
      val t = checkType(typename)

      for {
        _ <- addConstraint(TypesEqual(t, varType)(typename.getSource(), (a, e) => s"Cannot assign new id of type $a to variable of type $e."))
      } yield LazyBound { s =>
        t.subst(s) match {
          case it: IdType =>
            typed.NewIdStmt(source, varname, it)
          case _ =>
            addError(typename, s"Type $t must be declared as idType.")
            typed.makeBlock(source, List())
        }
      }
    case ReturnStmt(source, expr, assertions) =>
      for {
        exprTyped <- checkExpr(expr)
        _ <- addConstraint(TypesEqual(exprTyped.typ, ctxt.expectedReturn)(expr.getSource(), (a, e) => s"Cannot return $a from function with return type $e."))
        assertionsTyped <- assertions.traverse(checkAssertStatement)
      } yield LazyBound { s =>
        typed.ReturnStmt(source, exprTyped.bind(s), assertionsTyped.map(_.bind(s)))
      }
    case s: AssertStmt =>
      checkAssertStatement(s)
  }

  def checkAssertStatement(s: AssertStmt)(implicit ctxt: Context): TypeResult[LazyBound[typed.AssertStmt]] = {
    for {
      typedExpr <- checkExpr(s.expr)
      _ <- addConstraint(TypesEqual(typedExpr.typ, BoolType())(s.expr.getSource(), (a, e) => s"Assert must be of type Bool but found $a."))
    } yield LazyBound { subst =>
      typed.AssertStmt(source = s.source, expr = typedExpr.bind(subst))
    }
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

  def checkPattern(pattern: InExpr, expectedType: typed.InTypeExpr)(implicit ctxt: Context): TypeResult[(Context, LazyBound[typed.InExpr])] = {
    TypeResult(cs => {
      val ((ctxt2, cs2), r) = checkPattern2(pattern, expectedType).run((ctxt, cs)).value
      ((ctxt2, r), cs2)
    })
  }


  def checkPattern2(pattern: InExpr, expectedType: typed.InTypeExpr): State[(Context, TypeConstraints), LazyBound[typed.InExpr]] = {
    def getCtxt: State[(Context, TypeConstraints), Context] =
      State.get.map(_._1)

    def ctxtAddBinding(name: String, typ: typed.InTypeExpr): State[(Context, TypeConstraints), Unit] =
      State.modify { case (c, tc) =>
        (c.withBinding(name, typ), tc)
      }

    def skip: State[(Context, TypeConstraints), Unit] = State.pure(())

    def lift[T](s: TypeResult[T]): State[(Context, TypeConstraints), T] =
      State { x =>
        val (cs, r) = s.run(x._2)

        ((x._1, cs), r)
      }

    def errorResult[S]: State[S, LazyBound[TypedAst.InExpr]] =
      State.pure[S, LazyBound[typed.InExpr]](LazyBound { _ =>
        typed.VarUse(pattern.getSource(), AnyType(), "_")
      })

    pattern match {
      case VarUse(source, name) =>
        for {
          ctxt <- getCtxt
          _ <- if (ctxt.types.contains(name)) {
            addError(pattern, s"Variable with name $name is already defined.")
            skip
          } else {
            ctxtAddBinding (name, expectedType)
          }
        } yield {
          LazyBound { subst =>
            typed.VarUse(source, expectedType.subst(subst), name)
          }
        }
      case f@FunctionCall(source, functionName, args) =>
        getCtxt.flatMap { ctxt: Context =>
          ctxt.types.get(functionName.name) match {
            case Some(PrincipleType(typeParams, typed.FunctionType(argTypes, returnType, FunctionKindDatatypeConstructor()))) =>
              // check if args have same length
              if (args.length != argTypes.length) {
                addError(f, s"Wrong number of arguments: expected ${argTypes.length} but found ${args.length} arguments.")
              }


              for {
                // create new variables for typeParams
                tvs <- typeParams.traverse(s => lift(freshVar(s.name)))
                // substitute new type params in argTypes and returnType
                subst = Subst(typeParams.zip(tvs).toMap)
                argTypes2 = argTypes.map(_.subst(subst))
                returnType2 = returnType.subst(subst)

                // add new constraints: expected type must be equal to returnType
                _ <- lift(addConstraint(TypesEqual(expectedType, returnType2)(f.getSource())))

                // match args patterns recursively
                typedArgs <- args.zip(argTypes2).traverse(e => checkPattern2(e._1, e._2))

              } yield LazyBound { subst =>
                typed.FunctionCall(
                  source,
                  returnType2.subst(subst),
                  functionName,
                  typedArgs.map(_.bind(subst)),
                  FunctionKindDatatypeConstructor()
                )
              }

            case Some(_) =>
              addError(f, s"Function $functionName is not a datatype constructor.")
              errorResult
            case None =>
              addError(f, s"Could not find pattern $functionName.")
              errorResult
          }
        }
      case f =>
        addError(pattern, s"Pattern not supported: $pattern")
        errorResult
    }
  }

  /**
   * Function types of builtin functions
   * (list of functions because there can be overloads or builtins that are not usable in the source language)
   */
  def builtinTypes(bf: BuiltInFunc)(implicit ctxt: Context): List[PrincipleType] = {
    implicit def f(e: (List[typed.InTypeExpr], typed.InTypeExpr)): typed.FunctionType =
      typed.FunctionType(e._1, e._2, FunctionKindDatatypeConstructor())()

    implicit def p(e: (List[typed.InTypeExpr], typed.InTypeExpr)): PrincipleType =
      pf(f(e))

    implicit def pf(e: typed.FunctionType): PrincipleType =
      PrincipleType(List(), e)

    implicit def s(p: PrincipleType): List[PrincipleType] = List(p)

    val t = TypeVarUse("T")()
    bf match {
      case BF_isVisible() => p(List(CallIdType()) -> BoolType())
      case BF_happensBefore(HappensBeforeOn.Unknown()) => List(
        p(List(CallIdType(), CallIdType()) -> BoolType()),
        p(List(InvocationIdType(), InvocationIdType()) -> BoolType())
      )
      case BF_distinct() => List()
      case BF_sameTransaction() => p(List(CallIdType(), CallIdType()) -> BoolType())
      case BF_less() => p(List(IntType(), IntType()) -> BoolType())
      case BF_lessEq() => p(List(IntType(), IntType()) -> BoolType())
      case BF_greater() => p(List(IntType(), IntType()) -> BoolType())
      case BF_greaterEq() => p(List(IntType(), IntType()) -> BoolType())
      case BF_equals() => PrincipleType(List(t), List(t, t) -> BoolType())
      case BF_notEquals() => PrincipleType(List(t), List(t, t) -> BoolType())
      case BF_and() => p(List(BoolType(), BoolType()) -> BoolType())
      case BF_or() => p(List(BoolType(), BoolType()) -> BoolType())
      case BF_implies() => p(List(BoolType(), BoolType()) -> BoolType())
      case BF_plus() => p(List(IntType(), IntType()) -> IntType())
      case BF_minus() => p(List(IntType(), IntType()) -> IntType())
      case BF_mult() => p(List(IntType(), IntType()) -> IntType())
      case BF_div() => p(List(IntType(), IntType()) -> IntType())
      case BF_mod() => p(List(IntType(), IntType()) -> IntType())
      case BF_not() => p(List(BoolType()) -> BoolType())
      case BF_getOperation() => p(List(CallIdType()) -> ctxt.operationType.get)
      case BF_getInfo() => p(List(InvocationIdType()) -> InvocationInfoType())
      case BF_getResult() => p(List(InvocationIdType()) -> InvocationResultType())
      case BF_getOrigin() => List(
        p(List(CallIdType()) -> InvocationIdType()),
        p(List(TransactionIdType()) -> InvocationIdType())
      )
      case BF_getTransaction() => p(List(CallIdType()) -> TransactionIdType())
      case BF_inCurrentInvoc() => p(List(CallIdType()) -> BoolType())
    }
  }

  def instantiatePrincipleType(p: PrincipleType): TypeResult[typed.InTypeExpr] = {
    for {
      vars <- p.typeParams.traverse(p => for (v <- freshVar(p.name)) yield p -> v)
      subst = Subst(vars.toMap)
    } yield {
      p.typ.subst(subst)
    }

  }

  def checkExpr(e: InExpr)(implicit ctxt: Context): TypeResult[ExprResult] = {
    def pure(e: typed.InExpr): TypeResult[ExprResult] =
      TypeResult.pure(ExprResult(e.getTyp, e.getSource(), LazyBound { subst => e }))

    e match {
      case v@VarUse(source, name) =>
        val t = lookup(Identifier(source, name))
        for {
          t2 <- instantiatePrincipleType(t)
        } yield {
          val t3 = t2 match {
            case typed.FunctionType(List(), r, _) => r
            case other => other
          }
          ExprResult(t3, source, LazyBound { s =>
            val t4 = t3.subst(s) match {
              case t: TypedAst.FunctionType =>
                addError(source, s"Function call $name requires arguments.")
                t
              case other => other
            }
            typed.VarUse(source, t4, name)
          })
        }

      case BoolConst(source, v) =>
        pure(typed.BoolConst(source, BoolType(), v))
      case IntConst(source, value) =>
        pure(typed.IntConst(source, IntType(), value))
      case fc: FunctionCall =>
        checkFunctionCall(fc)
      case ab@ApplyBuiltin(source, function, args) =>
        val funcTypes = builtinTypes(function)

        for {
          typedArgs <- args.traverse(checkExpr)
          returnType <- freshVar(s"${function}_res")
          actualFuncType = typed.FunctionType(typedArgs.map(_.typ), returnType, FunctionKindDatatypeConstructor())()
          // instantiate type vars in principle types
          funcTypes2 <- funcTypes.traverse(instantiatePrincipleType)
          funcTypes3 = funcTypes2.map(_.asInstanceOf[typed.FunctionType])
          alternativeConstraints: List[TypeConstraint] = funcTypes3.map(ft => TypesEqual(actualFuncType, ft.asInstanceOf[TypedAst.FunctionType])(ab.source,
            (a, e) => s"Invalid function call. Expected $e and found $a."))
          _ <- addConstraint(alternativeConstraints.reduce(makeAlternative))
        } yield {
          ExprResult(returnType, source, LazyBound { subst =>
            val function2: BuiltInFunc = function match {
              case BF_happensBefore(HappensBeforeOn.Unknown()) =>
                val h =
                  if (typedArgs.head.typ.subst(subst) == typed.CallIdType())
                    HappensBeforeOn.Call()
                  else
                    HappensBeforeOn.Invoc()
                BF_happensBefore(h)
              case _ => function
            }

            typed.ApplyBuiltin(source, returnType.subst(subst), function2, typedArgs.map(_.bind(subst)))
          })
        }
      case qe@QuantifierExpr(source, quantifier, vars, expr) =>
        for {
          typedVars <- checkParamsC(vars)
          newCtxt = ctxt.copy(
            types = ctxt.types ++ getArgTypesT(typedVars)
          )
          exprTyped <- checkExpr(expr)(newCtxt)
          _ <- addConstraint(TypesEqual(exprTyped.typ, BoolType())(expr.getSource(), (a, e) => s"Quantifier expression must be of type Bool but found $a."))
        } yield {
          ExprResult(BoolType(), source, LazyBound { subst =>
            typed.QuantifierExpr(
              source = source,
              quantifier = quantifier,
              typ = BoolType(),
              vars = typedVars.map(_.subst(subst)),
              expr = exprTyped.bind(subst))
          })
        }
    }
  }

  def makeAlternative(left: TypeConstraint, right: TypeConstraint): TypeConstraint =
    Alternative(List(left), List(right))(left.trace)

  def checkCall(source: AstElem, funcName: String, typedArgs: List[(SourceTrace, typed.InTypeExpr)], paramTypes: List[typed.InTypeExpr]): TypeResult[Unit] = {
    if (paramTypes.length != typedArgs.length) {
      addError(source,
        s"""
           |Wrong call to function $funcName:
           |Expected parameters: ${paramTypes.mkString(", ")}
           |Actual parameters: ${typedArgs.map(a => a._2).mkString(", ")}""".stripMargin)
      TypeResult.pure(())
    } else {
      typedArgs.zip(paramTypes).traverse { case ((argSource, argT), paramT) =>
        addConstraint(TypesEqual(argT, paramT)(argSource, (a, e) => s"Wrong parameter when calling function $funcName: Expected $e but found $a."))
      }.map(_ => ())
    }
  }

  def getCrdtStructure(e: InExpr): QueryStructure = e match {
    case FunctionCall(_, name, args) =>
      QueryStructure(name.name, args.map(getCrdtStructure))
    case VarUse(_, name) =>
      QueryStructure(name, List())
    case _ =>
      QueryStructure(s"?<$e>", List())
  }

  def getQueryReturnType(fc: FunctionCall)(implicit ctxt: Context): Option[TypedAst.InTypeExpr] = {
    val crdt = ctxt.programCrdt.get
    try
      Some(crdt.queryReturnType(getCrdtStructure(fc)))
    catch {
      case t: Throwable =>
        //        addError(fc, s"Cannot get query type for $fc\n$t")
        None
    }
  }

  def checkFunctionCall(fc: FunctionCall)(implicit ctxt: Context): TypeResult[ExprResult] = {
    for {
      typedArgs <- fc.args.traverse(checkExpr)
      funcType <- instantiatePrincipleType(lookup(fc.functionName))
      r <- checkFunctionCall2(fc, typedArgs, funcType)
    } yield r
  }


  private def checkFunctionCall2(fc: FunctionCall, typedArgs: List[ExprResult], funcType: TypedAst.InTypeExpr)(implicit ctxt: Context): TypeResult[ExprResult] = {
    def makeFunc(newT: TypedAst.InTypeExpr, newKind: FunctionKind, subst: Subst) = {
      typed.FunctionCall(
        fc.source,
        newT.subst(subst),
        fc.functionName,
        typedArgs.map(_.bind(subst)),
        newKind
      )
    }

    funcType match {
      case typed.FunctionType(argTypes, returnType, kind) =>
        for {
          _ <- checkCall(fc, fc.functionName.name, typedArgs.map(e => e.source -> e.typ), argTypes)
          r <-
            if (returnType == ctxt.queryType.get) {
              for {
                v <- freshVar("qry")
                _ <- addConstraint {

                  getQueryReturnType(fc) match {
                    case Some(qrt) =>

                      def msg(a: String, e: String): String =
                        s"Query used in invalid context: Expected $qrt or ${ctxt.queryType.get}, but found $a"

                      makeAlternative(
                        TypesEqual(v, ctxt.queryType.get)(fc.source, msg),
                        TypesEqual(v, qrt)(fc.source, msg)
                      )
                    case None =>
                      TypesEqual(v, ctxt.queryType.get)(fc.source, (a, e) => s"Cannot use $fc as query. Expected $e but found $a.")
                  }
                }
              } yield {
                ExprResult(v, fc.source, LazyBound { subst =>
                  val rt = returnType.subst(subst)
                  val newKind =
                    if (rt == ctxt.queryType.get) FunctionKindDatatypeConstructor()
                    else FunctionKindCrdtQuery()
                  makeFunc(returnType, newKind, subst)
                })
              }
            } else {
              TypeResult.pure(ExprResult(returnType, fc.source, LazyBound { subst =>
                makeFunc(returnType, FunctionKindDatatypeConstructor(), subst)
              }))
            }
        } yield r
      case _ =>
        if (funcType != AnyType())
          addError(fc.functionName, s"${fc.functionName.name} is not a function.")
        val t = AnyType()
        TypeResult.pure(ExprResult(t, fc.source, LazyBound { subst =>
          makeFunc(t, FunctionKindDatatypeConstructor(), subst)
        }))
    }
  }
}

object Typer {

  class TypeErrorException(val trace: SourceTrace, val msg: String) extends RuntimeException(s"Error in line ${trace.getLine}: $msg") {
  }

  sealed abstract class TypeConstraint {
    def trace: SourceTrace
  }

  def cannotMatch(actual: String, expected: String): String =
    s"Could not match types $actual and $expected."

  case class TypesEqual(actual: typed.InTypeExpr, expected: typed.InTypeExpr)(val trace: SourceTrace, val message: (String, String) => String = cannotMatch) extends TypeConstraint

  case class Alternative(left: List[TypeConstraint], right: List[TypeConstraint])(val trace: SourceTrace) extends TypeConstraint

  case class TypeConstraintsSolutionError(
    source: SourceTrace,
    message: String
  )

  case class TypeConstraintsSolution(
    subst: Subst,
    errors: List[TypeConstraintsSolutionError] = List(),
    remainingConstraints: List[TypeConstraint] = List()
  ) {


  }

  case class TypeConstraints(
    constraints: List[TypeConstraint] = List(),
    varCount: Int = 0
  ) {
    def withConstraint(c: TypeConstraint): TypeConstraints = {
      copy(constraints = c :: constraints)
    }


    def freshVar(name: String): (TypeVarUse, TypeConstraints) =
      (TypeVarUse(s"?$name$varCount")(), copy(varCount = varCount + 1))


    def solve: TypeConstraintsSolution = {

      /** put alternatives at the end to avoid extensive backtracking */
      def tcSort(list: List[TypeConstraint]): List[TypeConstraint] = {
        val (normalConstraints, alternatives) = list.partition(!_.isInstanceOf[Alternative])
        normalConstraints.reverse ++ alternatives.reverse
      }

      /** try two alternatives */
      def fork(first: TypeConstraintsSolution, other: => TypeConstraintsSolution): TypeConstraintsSolution = {
        if (first.errors.isEmpty)
          first
        else if (other.errors.isEmpty)
          other
        else {
          // if both have errors continue with the one that has fewer remaining constraints
          val m = if (first.remainingConstraints.size < other.remainingConstraints.size) first else other
          val r = unify(m.remainingConstraints, m.subst)
          r.copy(errors = m.errors ++ r.errors)
        }
      }


      def unify(list: List[TypeConstraint], subst: Subst): TypeConstraintsSolution =
        list match {
          case List() => TypeConstraintsSolution(subst)
          case c :: cs =>
            c match {
              case tec@TypesEqual(left, right) =>
                def nestedMessage(e: String, a: String): String =
                  s"$subst\n${tec.message(left.subst(subst).toString, right.subst(subst).toString)}\nCould not match $a and $e."

                (left, right) match {
                  case _ if left == right =>
                    unify(cs, subst)
                  case (_: AnyType, _) | (_, _: AnyType) =>
                    // ignore any-constraints
                    unify(cs, subst)
                  case (v: TypeVarUse, r) =>
                    if (r.freeVars.contains(v))
                      TypeConstraintsSolution(
                        subst, List(TypeConstraintsSolutionError(tec.trace, s"Cannot match type $v with $r (failed occurs check)")), cs)
                    else
                      subst.get(v) match {
                        case Some(l) =>
                          // apply subst
                          unify(TypesEqual(l, r)(tec.trace, (a, e) => tec.message(a, e)) :: cs, subst)
                        case None =>
                          // add to subst
                          unify(cs, subst + (v -> r.subst(subst)))
                      }
                  case (t, v: TypeVarUse) =>
                    // swap
                    unify(TypesEqual(v, t)(tec.trace, (a, e) => tec.message(a, e)) :: cs, subst)
                  case (typed.FunctionType(as1, r1, _), typed.FunctionType(as2, r2, _)) if as1.length == as2.length =>
                    // unify type args:
                    unify(as1.zip(as2).map(e => TypesEqual(e._1, e._2)(tec.trace, nestedMessage)) ++
                      (TypesEqual(r1, r2)(tec.trace, nestedMessage) ::
                        cs), subst)
                  case (SimpleType(t1, as1), SimpleType(t2, as2)) if t1 == t2 && as1.length == as2.length =>
                    // unify type args:
                    unify(as1.zip(as2).map(e => TypesEqual(e._1, e._2)(tec.trace, nestedMessage)) ++ cs, subst)
                  case _ =>
                    TypeConstraintsSolution(
                      subst, List(TypeConstraintsSolutionError(tec.trace, tec.message(left.subst(subst).toString, right.subst(subst).toString))), cs)
                }


              case Alternative(left, right) =>
                fork(unify(left ++ cs, subst),
                  unify(right ++ cs, subst))
            }

        }

      val sortedConstraints = tcSort(constraints)

      //      println("Solving constraints: ")
      //      for (c <- sortedConstraints) {
      //        println(c)
      //      }

      unify(sortedConstraints, Subst())
    }
  }


}


