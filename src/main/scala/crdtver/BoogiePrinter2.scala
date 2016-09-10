//package crdtver
//
//import crdtver.BoogieAst._
//import gnieh.pp._
//
///**
//  * Created by peter on 18.08.16.
//  */
//class BoogiePrinter2 {
//
//
//
//
//
//  def printType(typ: TypeExpr): Doc = typ match {
//    case TypeBool() => "bool"
//    case MapType(argsTypes, resultType) =>
//      "[" :: argsTypes.map(printType).mkString(", ") :: "]" :: printType(resultType)
//    case FunctionType(argsTypes, resultType) =>
//      ???
//    case SimpleType(name) =>
//      name
//  }
//
//
//  def printVarDecl(decl: VarDecl): Doc =
//    decl.name :: ": " :: printType(decl.typ)
//
//  def printExpr(expr: Expr): Doc = expr match {
//    case BoolConst(false) =>
//      "false"
//    case IntConst(i) =>
//      i.toString
//    case BoolConst(true) =>
//      "true"
//    case BoolConst(false) =>
//      "false"
//    case IdentifierExpr(name) =>
//      name
//    case FunctionCall(name, args) =>
//      if (name.matches("[^a-zA-Z0-9]+") && args.size == 2) {
//        group("(" :: printExpr(args.head)
//          :: line
//          :: name
//          :: space
//          :: printExpr(args(1)) :: ")")
//      } else {
//
//
//        name :: "(" :: nest(2)(arglistDoc(args)) :: ")"
//      }
//    case Lookup(mapExpr, args) =>
//      printExpr(mapExpr) :: "[" :: nest(2)(arglistDoc(args)) :: "]"
//    case Forall(vars, e) =>
//      "(" :: "forall" :: space :: align((varsList(vars)) :: space :: "::" :: softline :: nest(2)(printExpr(e))) :: ")"
//    case Exists(vars, e) =>
//      "(" :: "forall" :: space :: align((varsList(vars)) :: space :: "::" :: softline :: nest(2)(printExpr(e))) :: ")"
//  }
//
//  def arglistDoc(args: List[Expr]): Doc = listDoc(args)(printExpr)
//
//  def varsList(args: List[VarDecl]): Doc = listDoc(args)(printVarDecl)
//
//  def listDoc[T](args: List[T])(printer: T => Doc): Doc = {
//    var argsDoc = empty
//    for ((a, i) <- args.zipWithIndex) {
//      if (i > 0) {
//        argsDoc = argsDoc :: "," :: softline
//      }
//      argsDoc = argsDoc :: printer(a)
//    }
//    argsDoc
//  }
//
//
//  def printStatement(s: Statement): Doc = s match {
//    case Block(List()) => "{}"
//    case Block(stmts) =>
//      var stmtsDoc = empty
//      val last = stmts.last
//      for (st <- stmts) {
//        stmtsDoc = stmtsDoc :: printStatement(st)
//        if (st != last) {
//          stmtsDoc = stmtsDoc :: line
//        }
//      }
//
//      "{" :: nest(2)(line :: stmtsDoc) :: line :: "}"
//    case LocalVar(name, typ) =>
//      "var " :: name :: ": " :: printType(typ) :: ";"
//    case IfStmt(condition, ifTrue, ifFalse) =>
//      val elsePart: Doc = ifFalse match {
//        case Block(List()) => empty
//        case _ =>
//          " else " :: printStatement(ifFalse)
//      }
//      "if (" :: printExpr(condition) :: ")" :: printStatement(ifTrue) :: elsePart
//
//    case ProcCall(resultVar, procname, arguments) =>
//      val start: Doc = resultVar match {
//        case Some(v) =>
//          "call " :: v :: " := "
//        case _ =>
//          "call "
//      }
//      start :: procname :: "(" :: nest(2)(arglistDoc(arguments)) :: ");"
//    case Assignment(variable, expr) =>
//      variable :: " := " :: printExpr(expr) :: ";"
//    case Assert(expr, attributes) =>
//      val attrDoc = if (attributes.nonEmpty) {
//        "{" :: listDoc(attributes)(printAttribute) :: "} "
//      } else {
//        empty
//      }
//      "assert " :: attrDoc :: printExpr(expr) :: ";"
//    case Assume(expr, attributes) =>
//      val attrDoc = if (attributes.nonEmpty) {
//        "{" :: listDoc(attributes)(printAttribute) :: "} "
//      } else {
//        empty
//      }
//      "assume " :: attrDoc :: printExpr(expr) :: ";"
//    case Havoc(v) =>
//      "havoc " :: v :: ";"
//    case Return(e) =>
//      "result := " :: printExpr(e) :: ";" :: line :: "return;"
//  }
//
//  def printAttribute(attribute: Attribute): Doc = {
//    ":" :: attribute.name :: " " :: attribute.arguments.map {
//      case Left(s) => "\"" + s + "\""
//      case Right(e) =>
//        printExpr(e)
//    }.mkString(", ")
//  }
//
//  def printDecl(decl: Declaration): Doc = decl match {
//    case TypeDecl(name, attributes) =>
//      var res = line :: "type "
//      if (attributes.nonEmpty) {
//        res = res :: "{" :: attributes.map(printAttribute).mkString(", ") :: "} "
//      }
//      res :: name :: ";"
//    case ConstantDecl(name, typ, isUnique) =>
//      ???
//    case FuncDecl(name, arguments, resultType, attributes, implementation) =>
//      var res = line :: "function "
//      if (attributes.nonEmpty) {
//        res = res :: "{" :: attributes.map(printAttribute).mkString(", ") :: "} "
//      }
//      res = res :: name :: "(" :: nest(2)(varsList(arguments)) :: "): " :: printType(resultType)
//
//      implementation match {
//        case None =>
//          res = res :: ";"
//        case Some(implExpr) =>
//          res = res :: "{" :: line :: nest(2)(printExpr(implExpr)) :: linebreak :: "}"
//      }
//
//      res
//    case GlobalVariable(name, typ) =>
//      "var " :: name :: ": " :: printType(typ) :: ";"
//    case Procedure(name, inParams, outParams, requires, modifies, ensures, body) =>
//      var res = line :: "procedure " :: name :: "(" :: nest(2)(varsList(inParams)) :: ")"
//      if (outParams.nonEmpty) {
//        res = res :: " returns (" :: nest(2)(varsList(outParams)) :: ")"
//      }
//      res = res :: line
//      if (modifies.nonEmpty) {
//        res = res :: "modifies " :: listDoc(modifies)(m => m.name) :: ";" :: line
//      }
//      for (r <- requires) {
//        if (r.isFree)
//          res = res :: "free "
//        res = res :: "requires " :: printExpr(r.condition) :: ";" :: line
//      }
//      for (r <- ensures) {
//        if (r.isFree)
//          res = res :: "free "
//        res = res :: "ensures " :: printExpr(r.condition) :: ";" :: line
//      }
//      res :: printStatement(body)
//    case Axiom(expr) =>
//      "axiom " :: printExpr(expr) :: ";"
//  }
//
//  def printProgram(prog: Program): Doc =  {
//    var res = empty
//    for (decl <- prog.declarations) {
//      res = res :: printDecl(decl) :: line
//    }
//    res
//  }
//
//}
