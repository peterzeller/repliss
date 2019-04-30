package crdtver.symbolic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import crdtver.utils.PrettyPrintDoc.{Doc, _}
import scalaz.Memo

import scala.collection.immutable.Set


object IsabelleTranslation {
  def createIsabelleDefs(name: String, datatypeImpl: SortDatatype => SortDatatypeImpl, constraints: List[NamedConstraint]): String =
    new IsabelleTranslation(datatypeImpl).createIsabelleDefs(name, constraints.reverse)

}

class IsabelleTranslation(datatypeImpl: SortDatatype => SortDatatypeImpl) {


  private var dataTypeDefinitions: List[Doc] = List()

  private val typeTranslation: SymbolicSort => Doc = Memo.mutableHashMapMemo {
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
  }


  private def translateUsedTypes(constraint: SVal[_ <: SymbolicSort]): Unit = {
    typeTranslation(constraint.typ)
    for (t <- constraint.childrenT)
      typeTranslation(t)

    for (c <- constraint.children)
      translateUsedTypes(c)

  }

  var fixedVars: Map[String, SymbolicSort] = Map()

  private def translateVal(v: SVal[_ <: SymbolicSort]): Doc = v match {
    case ConcreteVal(value) =>
      value.toString
    case SymbolicVariable(name, typ) =>
      val n = isabelleName(name)
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
      "NoReturn"
    case SMapGet(map, key) =>
      "(" <> translateVal(map) <+> translateVal(key) <> ")"
    case SymbolicMapVar(variable) =>
      translateVal(variable)
    case SymbolicMapEmpty(defaultValue) =>
      "(λ_. " <> translateVal(defaultValue) <> ")"
    case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
      "(" <> translateVal(baseMap) <> "(" <> translateVal(updatedKey) <+> ":=" <+> translateVal(newValue) <> "))"
    case SSetVar(variable) =>
      translateVal(variable)
    case SSetEmpty() =>
      "{}"
    case SSetInsert(SSetEmpty(), values) =>
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
      "(" <> q <> isabelleName(variable.name) <> "." <+> translateVal(body) <> ")"
    case SCommitted() =>
      "Committed"
    case SUncommitted() =>
      "Uncommitted"
    case SBool(value) =>
      value.toString
    case SNot(value) =>
      "¬" <> translateVal(value)
    case SAnd(left, right) =>
      group("(" <> translateVal(left) </> "∧" <+> translateVal(right) <> ")")
    case SOr(left, right) =>
      group("(" <> translateVal(left) </> "∨" <+> translateVal(right) <> ")")
    case SImplies(left, right) =>
      group("(" <> translateVal(left) </> "⟶" <+> translateVal(right) <> ")")
    case SFunctionCall(typ, functionName, args) =>
      group("(" <> functionName <+> nested(2, sep(line, args.map(translateVal))) <> ")")
    case SDatatypeValue(inType, constructorName, values, dtyp) =>
      group("(" <> constructorName <+> nested(2, sep(line, values.map(translateVal))) <> ")")
    case SCallInfo(operationName, args) =>
      group("(" <> operationName <+> nested(2, sep(line, args.map(translateVal))) <> ")")
    case SCallInfoNone() =>
      "no_call"
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

  private val forbidden = Set("value", "write")

  private def isabelleName(description: String): String = {
    var res = description

    while (res.startsWith("_") || forbidden.contains(res)) {
      res = "q" + res
    }



    res
  }

  private def createIsabelleDefs(name: String, constraints1: List[NamedConstraint]): String = {
    val constraints = uniqueNames(constraints1)


    val sb: StringBuilder = new StringBuilder()
    for (c <- constraints) {
      translateUsedTypes(c.constraint)
    }

    sb.append(
      s"""
        |theory ${isabelleName(name)}
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
      for (c <- constraints) yield c -> translateVal(c.constraint)

    for ((n,t) <- fixedVars) {
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
