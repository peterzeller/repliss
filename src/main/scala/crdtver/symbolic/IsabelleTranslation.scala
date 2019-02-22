package crdtver.symbolic

import crdtver.utils.PrettyPrintDoc.{Doc, _}
import scalaz.Memo


class IsabelleTranslation {


  private var dataTypeDefinitions: List[Doc] = List()

  private val typeTranslation: SymbolicSort => Doc = Memo.mutableHashMapMemo {
    case SortInt() =>
      "int"
    case SortBoolean() =>
      "bool"
    case SortCustomDt(typ) =>
      val constructors: List[Doc] =
        typ.constructors.values.toList
          .map(c => c.name <+> sep(" ",
            c.args.map(a => "(" <> a.name <> ": \"" <> typeTranslation(a.typ) <> "\")")))
      val d: Doc =
        "datatype" <+> typ.name <+> "=" </>
          nested(2, sep("| ", constructors))

      dataTypeDefinitions = d :: dataTypeDefinitions
      typ.name
    case SortCustomUninterpreted(name) =>
      val d: Doc =
        "datatype" <+> name <+> "=" <+> name <+> "nat"
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
    case SortTransactionStatus() =>
      val name = "TransactionStatus"
      val d: Doc =
        "datatype" <+> name <+> "= Committed | Uncommitted"
      dataTypeDefinitions = d :: dataTypeDefinitions
      name
    case SortInvocationId() =>
      val name = "InvocationId"
      val d: Doc =
        "datatype" <+> name <+> "=" <+> name <+> "nat"
      dataTypeDefinitions = d :: dataTypeDefinitions
      name
    case SortMap(keySort, valueSort) =>
      typeTranslation(keySort) <+> "->" <+> typeTranslation(valueSort)
    case SortSet(valueSort) =>
      typeTranslation(valueSort) <+> "set"
    case SortOption(valueSort) =>
      typeTranslation(valueSort) <+> "option"
    case SortCall() =>
      // TODO
      "CallInfo"
    case SortInvocationInfo() =>
      // TODO
      "InvocationInfo"
    case SortInvocationRes() =>
      // TODO
      "InvocationRes"
  }


  private def translateUsedTypes(constraint: SVal[_ <: SymbolicSort]): Unit = {
    typeTranslation(constraint.typ)
    for (t <- constraint.childrenT)
      typeTranslation(t)

    for (c <- constraint.children)
      translateUsedTypes(c)

  }

  def translateVal(v: SVal[_ <: SymbolicSort]): Doc = v match {
    case ConcreteVal(value) =>
      value.toString
    case SymbolicVariable(name, typ) =>
      name
    case SEq(left, right) =>
      group("(" <> translateVal(left) </> "=" <+> translateVal(right) <> ")")
    case SNotEq(left, right) =>
      group("(" <> translateVal(left) </> "!=" <+> translateVal(right) <> ")")
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
      group("(case " <> translateVal(option) <> " of " </> nested(2, "  Some " <> ifSomeVariable.name <> " => " <> translateVal(ifSome) </> "| None => " <> translateVal(ifNone)))
    case SReturnVal(methodName, value) =>
      "(" <> methodName <> "Res" <+> translateVal(value) <> ")"
    case SReturnValNone() =>
      "NoReturn"
    case SMapGet(map, key) =>
      ???
    case value: SymbolicMap[_, _] =>
      ???
    case value: SymbolicSet[_] =>
      ???
    case SSetContains(set, value) =>
      ???
    case QuantifierExpr(quantifier, variable, body) =>
      ???
    case SCommitted() =>
      ???
    case SUncommitted() =>
      ???
    case SBool(value) =>
      ???
    case SNot(value) =>
      ???
    case SAnd(left, right) =>
      ???
    case SOr(left, right) =>
      ???
    case SImplies(left, right) =>
      ???
    case SFunctionCall(typ, functionName, args) =>
      ???
    case SDatatypeValue(inType, constructorName, values, dtyp) =>
      ???
    case SCallInfo(operationName, args) =>
      ???
    case SCallInfoNone() =>
      ???
    case SInvocationInfo(procname, args) =>
      ???
    case SInvocationInfoNone() =>
      ???
    case MapDomain(map) =>
      ???
    case IsSubsetOf(left, right) =>
      ???
  }

  def createIsabelleDefs(constraints: List[NamedConstraint]): Unit = {
    for (c <- constraints) {
      translateUsedTypes(c.constraint)
    }

    for (dt <- dataTypeDefinitions.reverse) {
      println(dt.prettyStr(120))
      println()
    }

    for (c <- constraints.reverse) {
      val d: Doc = translateVal(c.constraint)
      println(
        s"""
           |assumes ${c.description}:\n
           |        \"${d.prettyStr(120).replace("\n", "\n         ")}\"
         """.stripMargin)
    }
  }

}
