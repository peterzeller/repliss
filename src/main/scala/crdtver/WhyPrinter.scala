package crdtver

import crdtver.BoogieAst._

import scala.collection.immutable.TreeMap

/**
  * Created by peter on 18.08.16.
  */
class WhyPrinter {


  val sb = new StringBuilder()
  var line: Int = 1
  var sourceMap = TreeMap[Int, Element]()


  private def app(str: String): Unit = {
    sb.append(str)
  }

  private def app(str: Any): Unit = {
    sb.append(str)
  }

  private def appLine(str: Any): Unit = {
    sb.append(str)
    sb.append("\n")
    line += 1
  }

  def trace(e: Element): Unit = {
    sourceMap += (line -> e)
  }

  def printType(typ: TypeExpr): String = typ match {
    case TypeBool() => "bool"
    case MapType(argsTypes, resultType) =>
      "[" + argsTypes.map(printType).mkString(", ") + "]" + printType(resultType)
    case FunctionType(argsTypes, resultType) =>
      ???
    case SimpleType(name) =>
      name
  }


  def printVarDecl(decl: VarDecl): String =
    decl.name + ": " + printType(decl.typ)

  def printExpr(expr: Expr): Unit = expr match {
    case BoolConst(false) =>
      app("false")
    case IntConst(i) =>
      app(i)
    case BoolConst(true) =>
      app("true")
    case IdentifierExpr(name) =>
      app(name)
    case FunctionCall(name, args) =>
      if (name.matches("[^a-zA-Z0-9]+") && args.size == 2) {
        app("(")
        printExpr(args.head)
        app(" ")
        app(name)
        app(" ")
        printExpr(args(1))
        app(")")
      } else {
        app("(")
        app(name)

        for (a <- args) {
          app(" ")
          printExpr(a)
        }

        app(")")
      }
    case Lookup(mapExpr, args) =>
      printExpr(mapExpr)
      app("[")

      for ((a, i) <- args.zipWithIndex) {
        if (i > 0)
          app(", ")
        printExpr(a)
      }

      app("]")
    case Forall(vars, e) =>
      app("(forall ")
      app(vars.map(printVarDecl).mkString(", "))
      app(" :: ")
      printExpr(e)
      app(")")
    case Exists(vars, e) =>
      app("(exists ")
      app(vars.map(printVarDecl).mkString(", "))
      app(" :: ")
      printExpr(e)
      app(")")
  }

  def printIndent(indent: Int): Unit = {
    if (indent > 0) {
      app("  ")
      printIndent(indent - 1)
    }
  }

  def printStatement(s: Statement, indent: Int): Unit = {
    trace(s)

    s match {
      case Block(stmts) =>
        appLine("{")
        for (st <- stmts) {
          printIndent(indent + 1)
          printStatement(st, indent + 1)
          appLine("")
        }
        printIndent(indent)
        app("}")
      case LocalVar(name, typ) =>
        app("let ")
        app(name)
        app(": ")
        app(printType(typ))
        app(" in ")
      case IfStmt(condition, ifTrue, ifFalse) =>
        app("if ")
        printExpr(condition)
        app(" then ")
        printStatement(ifTrue, indent)
        ifFalse match {
          case Block(List()) =>
          case _ =>
            app(" else ")
            printStatement(ifFalse, indent)
        }
      case NondetIf(alts) =>
        if (alts.nonEmpty) {
          app("if (*) ")
          for ((alt, i) <- alts.zipWithIndex) {
            printStatement(makeBlock(alt), indent)
            if (i < alts.size - 2) {
              app(" else if (*) ")
            } else if (i == alts.size-2) {
              app(" else ")
            }
          }
        }
      case ProcCall(resultVar, procname, arguments) =>
        resultVar match {
          case Some(v) =>
            app("call ")
            app(v)
            app(" := ")
          case _ =>
            app("call ")
        }
        app(procname)
        app("(")

        for ((a, i) <- arguments.zipWithIndex) {
          if (i > 0)
            app(", ")
          printExpr(a)
        }

        app(");")
      case Assignment(variable, expr) =>
        app(variable)
        app(" := ")
        printExpr(expr)
        app(";")
      case Assert(expr, attributes) =>
        app("assert ")
        if (attributes.nonEmpty) {
          app("{")
          app(attributes.map(printAttribute).mkString(", "))

          app("} ")
        }
        printExpr(expr)
        app(";")
      case Assume(expr, attributes) =>
        app("assume ")
        if (attributes.nonEmpty) {
          app("{")
          app(attributes.map(printAttribute).mkString(", "))

          app("} ")
        }
        printExpr(expr)
        app(";")
      case Havoc(v) =>
        app("havoc ")
        app(v)
        app(";")
      case Return(e) =>
        app("result := ")
        printExpr(e)
        app("; return;")


    }
  }



  def printAttribute(attribute: Attribute): String = {
    ":" + attribute.name + " " + attribute.arguments.map {
      case Left(s) => "\"" + s + "\""
      case Right(e) =>
        val sb = new StringBuilder
        printExpr(e)
        sb.toString
    }.mkString(", ")
  }

  def printDecl(decl: Declaration) = {
    trace(decl)
    decl match {
      case TypeDecl(name, attributes) =>
        appLine("")
        app("type ")
        if (attributes.nonEmpty) {
          app("{")
          app(attributes.map(printAttribute).mkString(", "))

          app("} ")
        }
        app(name)
        app(";")
      case ConstantDecl(name, typ, isUnique) =>
        ???
      case FuncDecl(name, arguments, resultType, attributes, implementation) =>
        appLine("")
        app("function ")
        if (attributes.nonEmpty) {
          app("{")
          app(attributes.map(printAttribute).mkString(", "))

          app("} ")
        }
        sb append name
        app("(")
        app(arguments.map(printVarDecl).mkString(", "))
        app("): ")
        app(printType(resultType))
        implementation match {
          case None =>
            app(";")
          case Some(implExpr) =>
            appLine("{")
            printExpr(implExpr)
            app("}")
        }


      case GlobalVariable(name, typ) =>
        app("var ")
        app(name)
        app(": ")
        app(printType(typ))
        app(";")
      case Procedure(name, inParams, outParams, requires, modifies, ensures, body) =>
        appLine("")
        app("procedure ")
        app(name)
        app("(")
        app(inParams.map(printVarDecl).mkString(", "))
        app(")")
        if (outParams.nonEmpty) {
          app(" returns (")
          app(outParams.map(printVarDecl).mkString(", "))
          app(")")
        }
        appLine("")
        if (modifies.nonEmpty) {
          app("modifies ")
          app(modifies.map(_.name).mkString(", "))
          appLine(";")
        }
        for (r <- requires) {
          trace(r.condition)
          if (r.isFree)
            app("free ")
          app("requires ")
          printExpr(r.condition)
          appLine(";")
        }
        for (r <- ensures) {
          trace(r.condition)
          if (r.isFree)
            app("free ")
          app("ensures ")
          printExpr(r.condition)
          appLine(";")
        }
        printStatement(body, 0)
      case Axiom(expr) =>
        app("axiom ")
        printExpr(expr)
        app(";")
    }
  }

  def printProgram(prog: Program): String = {
    for (decl <- prog.declarations) {
      printDecl(decl)
      appLine("")
    }
    sb.toString()
  }

}
