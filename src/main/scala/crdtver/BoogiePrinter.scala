package crdtver

import crdtver.BoogieAst._

/**
  * Created by peter on 18.08.16.
  */
class BoogiePrinter {




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

  def printExpr(expr: Expr, sb: StringBuilder): Unit = expr match {
    case BoolConst(false) =>
      sb.append("false")
    case IntConst(i) =>
      sb.append(i)
    case BoolConst(true) =>
      sb.append("true")
    case IdentifierExpr(name) =>
      sb.append(name)
    case FunctionCall(name, args) =>
      if (name.matches("[^a-zA-Z0-9]+") && args.size == 2) {
        sb.append("(")
        printExpr(args.head, sb)
        sb.append(" ")
        sb.append(name)
        sb.append(" ")
        printExpr(args(1), sb)
        sb.append(")")
      } else {
        sb.append(name)
        sb.append("(")

        for ((a, i) <- args.zipWithIndex) {
          if (i > 0)
            sb.append(", ")
          printExpr(a, sb)
        }

        sb.append(")")
      }
    case Lookup(mapExpr, args) =>
      printExpr(mapExpr, sb)
      sb.append("[")

      for ((a, i) <- args.zipWithIndex) {
        if (i > 0)
          sb.append(", ")
        printExpr(a, sb)
      }

      sb.append("]")
    case Forall(vars, e) =>
      sb.append("(forall ")
      sb.append(vars.map(printVarDecl).mkString(", "))
      sb.append(" :: ")
      printExpr(e, sb)
      sb.append(")")
    case Exists(vars, e) =>
      sb.append("(exists ")
      sb.append(vars.map(printVarDecl).mkString(", "))
      sb.append(" :: ")
      printExpr(e, sb)
      sb.append(")")
  }

  def printIndent(sb: StringBuilder, indent: Int): Unit = {
    if (indent > 0) {
      sb.append("  ")
      printIndent(sb, indent - 1)
    }
  }

  def printStatement(s: Statement, sb: StringBuilder, indent: Int): Unit = s match {
    case Block(stmts) =>
      sb.append("{\n")
      for (st <- stmts) {
        printIndent(sb, indent + 1)
        printStatement(st, sb, indent + 1)
        sb.append("\n")
      }
      printIndent(sb, indent)
      sb.append("}")
    case LocalVar(name, typ) =>
      sb.append("var ")
      sb.append(name)
      sb.append(": ")
      sb.append(printType(typ))
      sb.append(";")
    case IfStmt(condition, ifTrue, ifFalse) =>
      sb.append("if (")
      printExpr(condition, sb)
      sb.append(") ")
      printStatement(ifTrue, sb, indent)
      ifFalse match {
        case Block(List()) =>
        case _ =>
          sb.append(" else ")
          printStatement(ifFalse, sb, indent)
      }

    case ProcCall(resultVar, procname, arguments) =>
      resultVar match {
        case Some(v) =>
          sb.append(v)
          sb.append(" := call ")
        case _ =>
          sb.append("call ")
      }
      sb.append(procname)
      sb.append("(")

      for ((a, i) <- arguments.zipWithIndex) {
        if (i > 0)
          sb.append(", ")
        printExpr(a, sb)
      }

      sb.append(");")
    case Assignment(variable, expr) =>
      sb.append(variable)
      sb.append(" := ")
      printExpr(expr, sb)
      sb.append(";")
    case Assert(expr, attributes) =>
      sb.append("assert ")
      if (attributes.nonEmpty) {
        sb.append("{")
        sb.append(attributes.map(printAttribute).mkString(", "))

        sb.append("} ")
      }
      printExpr(expr, sb)
      sb.append(";")
    case Assume(expr, attributes) =>
      sb.append("assume ")
      if (attributes.nonEmpty) {
        sb.append("{")
        sb.append(attributes.map(printAttribute).mkString(", "))

        sb.append("} ")
      }
      printExpr(expr, sb)
      sb.append(";")
    case Havoc(v) =>
      sb.append("havoc ")
      sb.append(v)
      sb.append(";")
    case Return(e) =>
      sb.append("result := ")
      printExpr(e, sb)
      sb.append("; return;")


  }

  def printAttribute(attribute: Attribute): String = {
    ":" + attribute.name + " " + attribute.arguments.map {
      case Left(s) => "\"" + s + "\""
      case Right(e) =>
        val sb = new StringBuilder
        printExpr(e, sb)
        sb.toString
    }.mkString(", ")
  }

  def printDecl(decl: Declaration, sb: StringBuilder) = decl match {
    case TypeDecl(name, attributes) =>
      sb.append("\ntype ")
      if (attributes.nonEmpty) {
        sb.append("{")
        sb.append(attributes.map(printAttribute).mkString(", "))

        sb.append("} ")
      }
      sb.append(name)
      sb.append(";")
    case ConstantDecl(name, typ, isUnique) =>
      ???
    case FuncDecl(name, arguments, resultType, attributes, implementation) =>
      sb.append("\nfunction ")
      if (attributes.nonEmpty) {
        sb.append("{")
        sb.append(attributes.map(printAttribute).mkString(", "))

        sb.append("} ")
      }
      sb append name
      sb.append("(")
      sb.append(arguments.map(printVarDecl).mkString(", "))
      sb.append("): ")
      sb.append(printType(resultType))
      implementation match {
        case None =>
          sb.append(";")
        case Some(implExpr) =>
          sb.append("{\n")
          printExpr(implExpr, sb)
          sb.append("}")
      }


    case GlobalVariable(name, typ) =>
      sb.append("var ")
      sb.append(name)
      sb.append(": ")
      sb.append(printType(typ))
      sb.append(";")
    case Procedure(name, inParams, outParams, requires, modifies, ensures, body) =>
      sb.append("\nprocedure ")
      sb.append(name)
      sb.append("(")
      sb.append(inParams.map(printVarDecl).mkString(", "))
      sb.append(")")
      if (outParams.nonEmpty) {
        sb.append(" returns (")
        sb.append(outParams.map(printVarDecl).mkString(", "))
        sb.append(")")
      }
      sb.append("\n")
      if (modifies.nonEmpty) {
        sb.append("modifies ")
        sb.append(modifies.map(_.name).mkString(", "))
        sb.append(";\n")
      }
      for (r <- requires) {
        if (r.isFree)
          sb.append("free ")
        sb.append("requires ")
        printExpr(r.condition, sb)
        sb.append(";\n")
      }
      for (r <- ensures) {
        if (r.isFree)
          sb.append("free ")
        sb.append("ensures ")
        printExpr(r.condition, sb)
        sb.append(";\n")
      }
      printStatement(body, sb, 0)
    case Axiom(expr) =>
      sb.append("axiom ")
      printExpr(expr, sb)
      sb.append(";")
  }

  def printProgram(prog: Program, sb: StringBuilder) {
    for (decl <- prog.declarations) {
      printDecl(decl, sb)
      sb.append("\n")
    }
  }

}
