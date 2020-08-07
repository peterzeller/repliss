package crdtver.symbolic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import crdtver.symbolic.IsabelleTranslation.Context
import crdtver.utils.PrettyPrintDoc.{Doc, _}
import crdtver.utils.myMemo

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.language.higherKinds


object IsabelleTranslation {
  def createIsabelleDefs(name: String, datatypeImpl: SortDatatype => SortDatatypeImpl, constraints: List[NamedConstraint]): String =
    new IsabelleTranslation(datatypeImpl).createIsabelleDefs(name, constraints.reverse)


  private case class Context(boundVars: Set[SymbolicVariable[_ <: SymbolicSort]] = Set()) {
    def withVariable(variable: SymbolicVariable[_ <: SymbolicSort]): Context =
      copy(boundVars = boundVars + variable)

  }

}

class IsabelleTranslation(datatypeImpl: SortDatatype => SortDatatypeImpl) {


  private var dataTypeDefinitions: List[Doc] = List()

  private val typeTranslation: SymbolicSort => Doc = new myMemo({
    case SortInt() =>
      "int"
    case SortBoolean() =>
      "bool"
    case s: SortDatatype =>
      val typ = datatypeImpl(s)
      val constructors: List[Doc] =
        typ.constructors.values.toList
          .map(c => isabelleName(c.name) <+> sep(" ",
            c.args.map(a => "(" <> isabelleName(a.name) <> ": \"" <> typeTranslation(a.typ) <> "\")")))
      val d: Doc =
        "datatype" <+> isabelleName(typ.name) <+> "=" <>
          nested(2, line <> "  " <> sep(line <> "| ", constructors))

      dataTypeDefinitions = d :: dataTypeDefinitions
      isabelleName(typ.name)
    case SortCustomUninterpreted(name) =>
      val d: Doc =
        "datatype" <+> isabelleName(name) <+> "=" <+> isabelleName(name) <+> "nat"
      dataTypeDefinitions = d :: dataTypeDefinitions
      name
    case SortCallId() =>
      val name = "CallId"
      val d: Doc =
        "datatype" <+> name <+> "=" <+> name <+> "nat"
      dataTypeDefinitions = d :: dataTypeDefinitions
      name
    case SortTxId() =>
      val name = "TxId"
      val d: Doc =
        "datatype" <+> name <+> "=" <+> name <+> "nat"
      dataTypeDefinitions = d :: dataTypeDefinitions
      name
    case SortInvocationId() =>
      val name = "InvocationId"
      val d: Doc =
        "datatype" <+> name <+> "=" <+> name <+> "nat"
      dataTypeDefinitions = d :: dataTypeDefinitions
      name
    case SortMap(keySort, valueSort) =>
      typeTranslation(keySort) <+> "=>" <+> typeTranslation(valueSort)
    case SortSet(valueSort) =>
      typeTranslation(valueSort) <+> "set"
    case SortOption(valueSort) =>
      typeTranslation(valueSort) <+> "option"
    case SortAny() =>
      throw new RuntimeException("any not supported")
  })


  private def translateUsedTypes(constraint: SVal[_ <: SymbolicSort]): Unit = {
    typeTranslation(constraint.typ)
    for (t <- constraint.childrenT)
      typeTranslation(t)

    for (c <- constraint.children)
      translateUsedTypes(c)

  }

  var fixedVars: Map[String, SymbolicSort] = Map()


  private def translateVal(v: SVal[_ <: SymbolicSort])(implicit ctxt: Context): Doc = v match {
    case ConcreteVal(true) => "True"
    case ConcreteVal(false) => "False"
    case ConcreteVal(value) =>
      value.toString
    case v@SymbolicVariable(name, bound, typ) =>
      val n = isabelleName(name)
      if (!ctxt.boundVars.contains(v))
        fixedVars = fixedVars + (n -> typ)
      n
    case SEq(left, right) =>
      group("(" <> translateVal(left) </> "=" <+> translateVal(right) <> ")")
    case SNotEq(left, right) =>
      group("(" <> translateVal(left) </> "≠" <+> translateVal(right) <> ")")
    case SLessThan(left, right) =>
      group("(" <> translateVal(left) </> "<" <+> translateVal(right) <> ")")
    case SLessThanOrEqual(left, right) =>
      group("(" <> translateVal(left) </> "<=" <+> translateVal(right) <> ")")
    case SDistinct(values) =>
      group("distinct [" <> nested(2, sep(line <> ", ", values.map(translateVal)) <> "]"))
    case SNone(ofTyp) =>
      "None"
    case SSome(value) =>
      "(Some " <> translateVal(value) <> ")"
    case SOptionMatch(option, ifSomeVariable, ifSome, ifNone) =>
      group("(case " <> translateVal(option) <> " of " </> nested(2, "  Some " <> isabelleName(ifSomeVariable.name) <> " => " <> translateVal(ifSome) </> "| None => " <> translateVal(ifNone)) <> ")")
    case SReturnVal(methodName, value) =>
      "(" <> methodName <> "_res" <+> translateVal(value) <> ")"
    case SReturnValNone() =>
      "NoResult"
    case SMapGet(map, key) =>
      "(" <> translateVal(map) <+> translateVal(key) <> ")"
    case SymbolicMapEmpty(defaultValue) =>
      "(λ_. " <> translateVal(defaultValue) <> ")"
    case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
      "(" <> translateVal(baseMap) <> "(" <> translateVal(updatedKey) <+> ":=" <+> translateVal(newValue) <> "))"
    case SSetVar(variable) =>
      translateVal(variable)
    case SSetEmpty(_) =>
      "{}"
    case SSetInsert(SSetEmpty(_), values) =>
      "{" <> sep(", ", values.toList.map(translateVal)) <> "}"
    case SSetInsert(set, values) =>
      "(" <> translateVal(set) <+> "∪" <+> "{" <> sep(", ", values.toList.map(translateVal)) <> "})"
    case SSetUnion(set1, set2) =>
      "(" <> translateVal(set1) <+> "∪" <+> translateVal(set2) <> ")"
    case SSetContains(set, value) =>
      "(" <> translateVal(value) <+> "∈" <+> translateVal(set) <> ")"
    case QuantifierExpr(quantifier, variable, body) =>
      val q: Doc = quantifier match {
        case QForall() => "∀"
        case QExists() => "∃"
      }
      group("(" <> q <> isabelleName(variable.name) <> "." </> nested(2, translateVal(body)(ctxt.withVariable(variable))) <> ")")
    case SCommitted() =>
      "Committed"
    case SUncommitted() =>
      "Uncommitted"
    case SBool(value) =>
      value.toString
    case SNot(value) =>
      "¬" <> translateVal(value)
    case SAnd(left, right) =>
      group("(" <> translateVal(left) </> "∧" <+> nested(2, translateVal(right)) <> ")")
    case SOr(left, right) =>
      group("(" <> translateVal(left) </> "∨" <+> nested(2, translateVal(right)) <> ")")
    case SImplies(left, right) =>
      group("(" <> translateVal(left) </> "⟶" <+> nested(2, translateVal(right)) <> ")")
    case SFunctionCall(typ, functionName, args) =>
      group("(" <> functionName.name <+> nested(2, sep(line, args.map(translateVal))) <> ")")
    case SDatatypeValue(inType, constructorName, values, dtyp) =>
      group("(" <> constructorName <+> nested(2, sep(line, values.map(translateVal))) <> ")")
    case SCallInfo(operationName, args) =>
      group("(" <> operationName <+> nested(2, sep(line, args.map(translateVal))) <> ")")
    case SCallInfoNone() =>
      "NoCall"
    case SInvocationInfo(procname, args) =>
      group("(" <> isabelleName(procname) <+> nested(2, sep(line, args.map(translateVal))) <> ")")
    case SInvocationInfoNone() =>
      "no_invocation"
    case MapDomain(map) =>
      "(dom" <+> translateVal(map) <> ")"
    case IsSubsetOf(left, right) =>
      group("(" <> translateVal(left) </> "⊆" <+> translateVal(right) <> ")")
    case s: SValOpaque[_] =>
      throw new RuntimeException("SValOpaque not supported")
    case SNamedVal(name, value) =>
      "(*" <+> name <+> "*)" <+> translateVal(value)
    case SChooseSome(condition, value) =>
      // note: not semantically equivalent
      "(SOME " <> value.name <> ". " <> translateVal(condition) <> ")"
  }

  def uniqueNames(constraints: List[NamedConstraint]): List[NamedConstraint] = {
    var usedNames: Set[String] = Set()
    for (c <- constraints) yield {
      var name = isabelleName(c.description)
      var i = 1
      while (usedNames.contains(name)) {
        i += 1
        name = s"${c.description}_$i"
      }
      usedNames = usedNames + name
      c.copy(description = name)
    }
  }

  private val forbidden = Set("value", "write", "from",
    "!", "!!", "%", "(", ")", "+", ",", "/", ":", "::", ";", "<", "<=", "=", "==", "=>", "?", "[", "―", "≡", "↽", "⇀", "⇌", "⊆", "]", "abbrevs",
    "and", "assumes", "begin", "binder", "checking", "class_instance", "class_relation", "code_module", "constant", "constrains", "datatypes",
    "defines", "export_files", "export_prefix", "external_files", "file", "file_prefix", "fixes", "for", "functions", "if", "imports", "in",
    "includes", "infix", "infixl", "infixr", "is", "keywords", "module_name", "monos", "morphisms", "notes", "obtains", "open", "output",
    "overloaded", "parametric", "pervasive", "premises", "private", "qualified", "rewrites", "shows", "structure", "type_class",
    "type_constructor", "unchecked", "when", "where", "|",
    ".:", "..:", "ML", "ML_command", "ML_export", "ML_file", "ML_file_debug", "ML_file_no_debug", "ML_prf", "ML_val", "SML_export", "SML_file",
    "SML_file_debug", "SML_file_no_debug", "SML_import", "\\<", "abbreviation", "alias", "also", "apply", "apply_end", "assume", "attribute_setup",
    "axiomatization", "back", "bibtex_file", "bnf", "bundle", "by", "case", "chapter", "class", "class_deps", "codatatype", "code_datatype", "code_deps", "code_identifier", "code_monad", "code_pred", "code_printing", "code_reflect", "code_reserved", "code_thms", "coinductive",
    "coinductive_set", "compile_generated_files", "consider", "consts", "context", "copy_bnf", "corollary", "datatype", "datatype_compat",
    "declaration", "declare", "default_sort", "defer", "define", "definition", "done", "end", "experiment", "export_code", "export_generated_files",
    "external_file", "extract", "extract_type", "finally", "find_consts", "find_theorems", "find_unused_assms", "fix", "free_constructors", "from",
    "full_prf", "fun", "fun_cases", "function", "functor", "generate_file", "global_interpretation", "guess", "have", "help", "hence", "hide_class", "hide_const", "hide_fact", "hide_type", "include", "including", "inductive", "inductive_cases", "inductive_set", "inductive_simps",
    "instance", "instantiation", "interpret", "interpretation", "judgment", "lemma", "lemmas", "let", "lift_bnf", "lift_definition", "lifting_forget",
    "lifting_update", "local_setup", "locale", "locale_deps", "method_setup", "moreover", "named_theorems", "next", "nitpick", "nitpick_params",
    "no_notation", "no_syntax", "no_translations", "no_type_notation", "nonterminal", "notation", "note", "notepad", "nunchaku", "nunchaku_params",
    "obtain", "old_rep_datatype", "oops", "oracle", "overloading", "paragraph", "parse_ast_translation", "parse_translation", "partial_function",
    "prefer", "presume", "prf", "primcorec", "primcorecursive", "primrec", "print_ML_antiquotations", "print_abbrevs", "print_antiquotations",
    "print_ast_translation", "print_attributes", "print_bnfs", "print_bundles", "print_case_translations", "print_cases", "print_claset",
    "print_classes", "print_codeproc", "print_codesetup", "print_coercions", "print_commands", "print_context", "print_definitions",
    "print_defn_rules", "print_facts", "print_induct_rules", "print_inductives", "print_interps", "print_locale", "print_locales", "print_methods",
    "print_options", "print_orders", "print_quot_maps", "print_quotconsts", "print_quotients", "print_quotientsQ3", "print_quotmapsQ3",
    "print_record", "print_rules", "print_simpset", "print_state", "print_statement", "print_syntax", "print_term_bindings", "print_theorems",
    "print_theory", "print_trans_rules", "print_translation", "proof", "prop", "proposition", "qed", "quickcheck", "quickcheck_generator",
    "quickcheck_params", "quotient_definition", "quotient_type", "realizability", "realizers", "record", "schematic_goal", "section", "setup",
    "setup_lifting", "show", "simproc_setup", "sledgehammer", "sledgehammer_params", "smt_status", "solve_direct", "sorry", "specification",
    "subclass", "subgoal", "sublocale", "subparagraph", "subsection", "subsubsection", "supply", "syntax", "syntax_declaration", "term",
    "termination", "text", "text_raw", "then", "theorem", "theory", "thm", "thm_deps", "thm_oracles", "thus", "thy_deps", "translations", "try",
    "try0", "txt", "typ", "type_alias", "type_notation", "type_synonym", "typed_print_translation", "typedecl", "typedef", "ultimately", "unbundle",
    "unfolding", "unused_thms", "using", "value", "values", "welcome", "with", "write", "{:", "}:")

  private def isabelleName(description: String): String = {
    var res = description

    if (res.endsWith("_"))
      res = res + "q"

    while (res.startsWith("_") || forbidden.contains(res)) {
      res = "q" + res
    }


    res
  }

  case class GMap[B, K[_ <: B], V[_ <: B]](map: Map[Any, Any] = Map()) {
    def contains(key: K[_]): Boolean =
      map.contains(key)

    def get[T <: B](key: K[T]): Option[V[T]] =
      map.get(key).asInstanceOf[Option[V[T]]]

    def +[T <: B](kv: (K[T], V[T])): GMap[B, K, V] = {
      val newMap: Map[Any, Any] = map + kv
      GMap[B, K, V](newMap)
    }


  }


  private def createIsabelleDefs(name: String, constraints1: List[NamedConstraint]): String = {
    val constraints = uniqueNames(Simplifier.extractNamedValues(constraints1))


    val sb: StringBuilder = new StringBuilder()
    for (c <- constraints) {
      translateUsedTypes(c.constraint)
    }

    sb.append(
      s"""
         |theory "${isabelleName(name)}"
         |  imports Main
         |begin
         |
      """.stripMargin)

    for (dt <- dataTypeDefinitions.reverse) {
      sb.append(dt.prettyStr(120))
      sb.append("\n\n")
    }

    sb.append(s"""lemma "$name":""")

    val translatedConstraints: List[(NamedConstraint, Doc)] =
      for (c <- constraints) yield c -> translateVal(c.constraint)(Context())

    for ((n, t) <- fixedVars) {
      sb.append(
        s"""
           |fixes $n :: "${typeTranslation(t).prettyStr(120)}"
         """.stripMargin)
    }

    for ((c, d) <- translatedConstraints) {
      sb.append(
        s"""
           |assumes ${isabelleName(c.description)}:\n
           |        \"${d.prettyStr(120).replace("\n", "\n|         ")}\"
         """.stripMargin)
    }
    sb.append("shows False\n")

    sb.toString()
  }

}
