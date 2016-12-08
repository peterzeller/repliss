package crdtver

import crdtver.WhyAst._
import PrettyPrintDoc._

/**
  * Created by peter on 18.08.16.
  */
class WhyPrinter {


  implicit def identToDoc(i: LIdent): Doc = text(i.name)

  implicit def identToDoc(i: UIdent): Doc = text(i.name)



  def printProgramDoc(prog: Module): Doc = {
    val declarations: List[MDecl] = sortDecls(prog.declarations)

    "module" <+> prog.name.name </> "" </>
      nested(1, declarations.map(d => printDecl(d) <> line <> line)) </>
      "end"
  }

  def findUsedNames(decl: MDecl): Set[String] = {
    var res = Set[String]()
    walk(decl) {
      case TypeSymbol(name, typeArgs) =>
        res += name.toString
    }
    res
  }

  def sortDecls(declarations: List[MDecl]): List[MDecl] = {
    sortDecls(declarations, declarations.flatMap(_.definedNames()).toSet)
  }

  def sortDecls(declarations: List[MDecl], definedNames: Set[String]): List[MDecl] = {
    declarations match {
      case Nil => Nil
      case decl::decls =>
        val newDefined = definedNames -- decl.definedNames()
        val usedNames: Set[String] = findUsedNames(decl)
        if (newDefined.intersect(usedNames).isEmpty) {
          // everything already defined:
          decl :: sortDecls(decls, newDefined)
        } else {
          decls.find(d => d.definedNames().exists(usedNames)) match {
            case Some(d) =>
              sortDecls(d :: decl :: decls.filter(_ != d), definedNames)
            case None =>
              decl :: sortDecls(decls, newDefined)
          }
        }
    }
  }


  def printFunBody(body: FunBody): Doc = {
    val bodyParams: List[TypedParam] = body.params
    val returnType: Option[TypeExpression] = body.returnType
    val specs: List[Spec] = body.specs
    printSignature(bodyParams, returnType, specs, " = " <+> printTerm(body.body))
  }

  def printSignature(bodyParams: List[TypedParam], returnType: Option[TypeExpression], specs: List[Spec], afterBody: Doc): Doc = {
    val params: Doc = sep(NilDoc(), bodyParams.map(line <> printTypedParam(_)))
    nested(2,
      nested(4, group(params <>
        (returnType match {
          case Some(t) => line <> ":" <+> printTypeExpr(t)
          case None => NilDoc()
        }))) </>

        sep(NilDoc(), specs.map(printSpec(_) <> line)) <>
        afterBody
    )
  }

  def printSpec(spec: Spec): Doc = spec match {
    case Requires(formula) =>
      "requires {" <+> printTerm(formula) <+> "}"
    case Ensures(formula) =>
      "ensures {" <+> printTerm(formula) <+> "}"
    case Returns(cases) =>
      "returns {" <+> cases.toString() <+> "}"
    case Reads(terms) =>
      "reads {" <+> sep(", ", terms.map(printTerm)) <+> "}"
    case Writes(terms) =>
      "writes {" <+> sep(", ", terms.map(printTerm)) <+> "}"
    case RaisesName(raised) =>
      "raises {" <+> sep(", ", raised.map("" <> _.toString())) <+> "}"
    case Raises(cases) =>
      "raises {" <+> cases.toString() <+> "}"
    case Variant(variants) =>
      "variant {" <+> variants.toString() <+> "}"
  }


  def printPattern(pattern: Pattern): Doc = pattern match {
    case OrPattern(patterns) =>
      ???
    case TuplePattern(patterns) =>
      "(" <> sep(", ", patterns.map(printPattern)) <> ")"
    case CatchAllPattern() =>
      "_"
    case VariablePattern(name) =>
      name.toString
    case ConstructorPattern(constructorName, args) =>
      constructorName.toString <> sep(NilDoc(), args.map(printPattern))
  }

  def printTerm(term: Term): Doc = term match {
    case IntConst(value) => value.toString()
    case RealConstant(value) => value.toString()
    case BoolConst(value) => value.toString
    case Symbol(name) => name.toString
    case FunctionCall(funcName, args) =>
      if (funcName.name.name.charAt(0).isLetter) {
        // function
        "(" <> funcName.toString <> sep(NilDoc(), args.map(" " <> printTerm(_))) <> ")"
      } else if (args.size == 2) {
        val opName = funcName

        def collectSameOps(t: Term): List[Term] = t match {
          case FunctionCall(opName2, List(l, r)) if opName == opName2 =>
            collectSameOps(l) ++ collectSameOps(r)
          case _ =>
            List(t)
        }

        val args2 = collectSameOps(args(0)) ++ collectSameOps(args(1))

        val opNameStr: String = opName.toString
        val printedArgs = args2.map(printTerm)

        //        ("(" <> sep(" " <> opName.toString <> " ", printedArgs) <> ")") :<|>
        //          (() => "(" <> nested(2, line <+> sep(line <> opName.toString, printedArgs.map(nested(2, _)))) <> ")")

        //        ("(" <> sep(" " <> opName.toString <> " ", printedArgs) <> ")")

        ("(" <> sep(" " <> opName.toString <> " ", printedArgs) <> ")") :<|>
          (() => "(" <> sep(line <> opNameStr <> " ", printedArgs.map(arg => nested(1 + opNameStr.length, arg))) <> ")")
      } else if (args.size == 1) {
        "(" <> funcName.toString <+> printTerm(args(0)) <> ")"
      } else {
        ???
      }
    case ArrayLookup(arrayTerm, indexTerm) =>
      ???
    case ArrayUpdate(arrayTerm, indexTerm, newValue) =>
      ???
    case Conditional(condition, ifTrue, ifFalse) =>
      // TODO short version without newline when used as expression?
      "if" <+> printTerm(condition) <+> "then begin" <> nested(4, line <> printTerm(ifTrue)) </> "end else begin" <> nested(4, line <> printTerm(ifFalse)) </> "end"
    case LetTerm(pattern, value, body) =>
      // TODO linebreak after in?
      "let" <+> printPattern(pattern) <+> "=" <+> printTerm(value) <+> "in" </> printTerm(body)

    case Sequence(terms) =>
      sep(";" <> line, terms.map(printTerm))
    case Loop(invariants, variant, body) =>
      ???
    case While(condition, invariants, variant, body) =>
      ???
    case MatchTerm(terms, cases) =>
      ???
    case QuantifierTerm(quantifier, binders, body) =>
      val q = quantifier match {
        case Forall() => "forall"
        case Exists() => "exists"
      }
      val front: Doc = q <+> sep(", ", binders.map(printTypedParamNoParen)) <> "."
      val bod: Doc = printTerm(body)
      "(" <> front <> ((" " <> bod) :<|> (() => nested(4, line <> bod))) <> ")"
    case Tuple(values) =>
      "(" <> sep(", ", values.map(printTerm)) <> ")"
    case RecordTerm(fields) =>
      ???
    case FieldAccess(recordTerm, fieldName) =>
      ???
    case FieldAssignment(recordTerm, fieldName, newValue) =>
      ???
    case FieldUpdate(recordTerm, fieldUpdates) =>
      ???
    case CastTerm(term, typ) =>
      ???
    case LabeledTerm(label, term) =>
      ???
    case CodeMark(name) =>
      ???
    case Old(term) =>
      "old" <+> printTerm(term)
    case Assert(formula) =>
      "(assert " <+> printTerm(formula) <> ")"
    case Assume(formula) =>
      "(assume " <+> printTerm(formula) <> ")"
    case Check(formula) =>
      "(check " <+> printTerm(formula) <> ")"
  }

  def printFunDefn(d: FunDefn): Doc = {
    d.name <+> printFunBody(d.body)
  }

  def printDecl(decl: MDecl): Doc = decl match {
    case GlobalLet(name, funBody, labels, isGhost) =>
      "let " <> name.name <+> printFunBody(funBody)
    case GlobalLetRec(recDefn) =>
      "let rec" <+> sep(line <> "with ", recDefn.map(printFunDefn))
    case GlobalVariable(name, typ, isGhost, labels) =>
      "val" <+> name.name <> ":" <+> printTypeExpr(typ)
    case AbstractFunction(isGhost, name, labels, params, returnType, specs) =>
      "val" <+> name.name <> printSignature(params, Some(returnType), specs, NilDoc())
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
      "use import" <+> name.toString
    case Namespace(name, declarations) =>
      decl.toString
  }

  def printTypeDefn(definition: TypeDefn): Doc = definition match {
    case AbstractType() =>
      NilDoc()
    case AliasType(alias) =>
      definition.toString
    case AlgebraicType(cases, invariants) =>
      "=" <+> nested(2, line <> "  " <> sep(line <> "| ", cases.map(p => printCase(p))))
    case RecordType(fields, invariants) =>
      definition.toString
    case RecordField(name, labels, typ, isGhost, isMutable) =>
      definition.toString
  }

  def printCase(c: TypeCase): Doc =
    c.name <+> sep(" ", c.paramsTypes.map(p => printTypeExpr(p.typ)))


  def printTypedParam(p: TypedParam): Doc =
    "(" <> p.name <> ":" <+> printTypeExpr(p.typ) <> ")"

  def printTypedParamNoParen(p: TypedParam): Doc =
    p.name <> ":" <+> printTypeExpr(p.typ)

  def printTypeExpr(t: TypeExpression): Doc = t match {
    case TypeSymbol(name, typeArgs) =>
      name.toString <> sep(NilDoc(), typeArgs.map(" " <> printTypeExpr(_)))
    case TypeVariable(name) =>
      name
    case TupleType(types) =>
      "(" <> sep(", ", types.map(printTypeExpr)) <> ")"
  }

  def printTypeDecl(decl: TypeDecl): Doc = {
    "type" <+> decl.name <+> printTypeDefn(decl.definition)
  }

  def printProgram(prog: Module): String = {
    printProgramDoc(prog).prettyStr(120)
  }

}
