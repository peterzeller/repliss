package crdtver

import crdtver.WhyAst._
import PrettyPrintDoc._

/**
  * Created by peter on 18.08.16.
  */
class WhyPrinter {


  implicit def identToDoc(i: LIdent): Doc = text(i.name)
  implicit def identToDoc(i: UIdent): Doc = text(i.name)

  def printProgramDoc(prog: Module): Doc =
    "module" <+> prog.name.name </>
      nested(1, prog.declarations.map(d => printDecl(d) <> line <> line)) </>
      "end"

  def printDecl(decl: MDecl): Doc = decl match {
    case GlobalLet(name, funBody, labels, isGhost) =>
      "let " <> name.name
    case GlobalLetRec(recDefn) =>
      decl.toString
    case GlobalVariable(name, typ, isGhost, labels) =>
      decl.toString
    case AbstractFunction(isGhost, name, labels, params, returnType, specs) =>
      decl.toString
    case ExceptionDecl(name, labels, typ) =>
      decl.toString
    case TypeDecls(decls) =>
      decls.map(printTypeDecl)
    case ConstantDecl(name, labels, typ, value) =>
      decl.toString
    case LogicDecls(decls) =>
      decl.toString
    case InductiveDecls(isCoinductive, decls) =>
      decl.toString
    case Axiom(name, formula) =>
      decl.toString
    case Lemma(name, formula) =>
      decl.toString
    case Goal(name, formula) =>
      decl.toString
    case Import(isClone, impExp, name, as, substitutions) =>
      decl.toString
    case Namespace(name, declarations) =>
      decl.toString
  }

  def printTypeDefn(definition: TypeDefn): Doc = definition match {
    case AbstractType() =>
      definition.toString
    case AliasType(alias) =>
      definition.toString
    case AlgebraicType(cases, invariants) =>
      nested(2, line <> "  " <> sep(line <> "| ",  cases.map(p => printCase(p))))
    case RecordType(fields, invariants) =>
      definition.toString
    case RecordField(name, labels, typ, isGhost, isMutable) =>
      definition.toString
  }

  def printCase(c: TypeCase): Doc =
    c.name <+> sep(" ", c.paramsTypes.map(printTypedParam))


  def printTypedParam(p: TypedParam): Doc =
    "(" <> p.name <> ":" <+> printTypeExpr(p.typ) <> ")"

  def printTypeExpr(t: TypeExpression): Doc = t match {
    case TypeSymbol(name, typeArgs) =>
      name.toString
    case TypeVariable(name) =>
      name
    case TupleType(types) =>
      "(" <> sep(", ", types.map(printTypeExpr)) <> ")"
  }

  def printTypeDecl(decl: TypeDecl): Doc = {
    "type" <+> decl.name <+> "=" <+> printTypeDefn(decl.definition)
  }

  def printProgram(prog: Module): String = {
    printProgramDoc(prog).prettyStr(120)
  }

}
