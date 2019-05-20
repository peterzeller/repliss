package crdtver.symbolic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import crdtver.language.crdts.UniqueName
import crdtver.symbolic.IsabelleTranslation.Context
import crdtver.utils.PrettyPrintDoc.{Doc, _}
import scalaz.Memo

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}


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

  private implicit def uniqueNameToDoc(uniqueName: UniqueName): Doc =
    uniqueName.toString


  private def translateVal(v: SVal[_ <: SymbolicSort])(implicit ctxt: Context): Doc = v match {
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
      group("(" <> functionName <+> nested(2, sep(line, args.map(translateVal))) <> ")")
    case SDatatypeValue(inType, constructorName, values, dtyp) =>
      group("(" <> constructorName <+> nested(2, sep(line, values.map(translateVal))) <> ")")
    case SCallInfo(operation) =>
      group("some_call(" <> translateVal(operation) <> ")")
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
    case SNamedVal(name, value) =>
      "(*" <+> name <+> "*)" <+> translateVal(value)
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

  private val forbidden = Set("value", "write", "from")

  private def isabelleName(description: UniqueName): String =
    isabelleName(description.toString)

  private def isabelleName(description: String): String = {
    var res = description

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

  def freeVars(value: SVal[_ <: SymbolicSort], bound: Set[SymbolicVariable[_ <: SymbolicSort]]): Set[SymbolicVariable[_ <: SymbolicSort]] = {
    value match {
      case v@SymbolicVariable(name, isBound, typ) =>
        if (isBound && !bound.contains(v))
          Set(v)
        else
          Set()
      case QuantifierExpr(quantifier, variable, body) =>
        freeVars(body, bound + variable)
      case _ =>
        value.children.flatMap(freeVars(_, bound)).toSet
    }
  }

  private def extractNamedValues(constraints1: List[NamedConstraint]): List[NamedConstraint] = {
    var usedVarnames: Set[String] = Set()
    var extracted = Map[SNamedVal[_ <: SymbolicSort], SymbolicVariable[_ <: SymbolicSort]]()
    val result = mutable.ListBuffer[NamedConstraint]()

    for (c <- constraints1) {
      Simplifier.rewrite({
        case v@SymbolicVariable(name, _, t) =>
          usedVarnames += name
          v
      })(c.constraint)
    }

    def makeVar[T <: SymbolicSort](name: String, t: T): SymbolicVariable[T] = {
      var name2 = name
      var i = 0
      while (usedVarnames.contains(name2)) {
        i += 1
        name2 = name + i
      }
      usedVarnames += name2
      SymbolicVariable(name2, false, t)
    }

    def extr(c: NamedConstraint): Unit = {
      val e2 = Simplifier.rewrite({
        case nv@SNamedVal(name, value) =>
          extracted.get(nv) match {
            case Some(v) =>
              v
            case None =>
              val vfreeVars: List[SymbolicVariable[_ <: SymbolicSort]] = freeVars(value, Set()).toList
              var t: SymbolicSort = value.typ
              for (fv <- vfreeVars.reverse) {
                t = SortMap(fv.typ, t)
              }
              val v = makeVar(name, t)
              extracted = extracted + (nv -> v)

              var left: SVal[SymbolicSort] = v
              for (fv <- vfreeVars) {
                left = SMapGet(left.cast[SortMap[SymbolicSort, SymbolicSort]], fv.cast[SymbolicSort])
              }

              result += NamedConstraint("${nv.name}_def", SVal.forallL(vfreeVars,  SEq(left, value.cast[SymbolicSort])))
              left
          }
      })(c.constraint)
      result += NamedConstraint(c.description, e2)
    }

    for (c <- constraints1)
      extr(c)
    return result.toList

  }

  private def createIsabelleDefs(name: String, constraints1: List[NamedConstraint]): String = {
    val constraints = uniqueNames(extractNamedValues(constraints1))


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
