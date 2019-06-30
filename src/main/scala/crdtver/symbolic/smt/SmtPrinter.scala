package crdtver.symbolic.smt

import crdtver.symbolic.smt.Smt._
import crdtver.utils.PrettyPrintDoc.Doc

import scala.collection.mutable

object SmtPrinter {


  case class PrintContext(
    var definitions: List[(String, Doc)] = List()

  ) {
    def addDefinition(name: String, doc: Doc): Unit = {
      if (!definitions.exists(_._1 == name)) {
        definitions = definitions ++ List(name -> doc)
      }
    }

  }

  /** prints this expression as Scala objects */
  def printScala(cs1: List[NamedConstraint]): Doc = {
    val cs = for ((c,i) <- cs1.zipWithIndex) yield c.copy(description = s"a${i}_${c.description}")

    import crdtver.utils.PrettyPrintDoc._

    val context = PrintContext()
    val docs = for (c <- cs) yield
      "val" <+> c.description <+> "=" <+> nested(4, SmtPrinter.printScala(c.constraint, context))

    sep(line, context.definitions.map(e => "val" <+> e._1 <+> "=" <+> e._2)) </>
      sep(line, docs) </>
      s"val assertions = List(${cs.map(_.description).mkString(", ")})"

  }


  def printScala(expr: SmtExpr, printContext: PrintContext): Doc = {
    import crdtver.utils.PrettyPrintDoc._


    def printPart(part: Any): Doc = part match {
      case e: SmtExpr => printScala(e, printContext)
      case s: String => "\"" <> s <> "\""
      case t: Type =>
        printType(t)
      case q: Quantifier => q.toString
      case l: List[t] =>
        print("List", l)
      case DatatypeConstructor(name, args) =>
        print("DatatypeConstructor", List(name, args))
      case a: Array[_] =>
        print("Array", a.toList)
      case b: Boolean => b.toString
      case b: BigInt => b.toString
      case _ =>
        throw new RuntimeException(s"unhandled case ${part.getClass}: $part")
    }

    def print(name: String, parts: List[Any]): Doc =
      group(name <> nested(2, "(" </> sep("," <> line, parts.map(printPart)) <> ")"))

    def printType(t: Smt.Type): Doc = {
      t match {
        case Sort(name) =>
          printContext.addDefinition(s"t_$name", print("Sort", List(name)))
          s"t_$name"
        case Datatype(name, constructors) =>
          printContext.addDefinition(s"t_$name", print("Datatype", List(name, constructors)))
          s"t_$name"
        case IntegerType() =>
          "IntegerType()"
        case BoolType() =>
          "BoolType()"
        case ArrayType(keyType, valueType) =>
          print("ArrayType", List(keyType, valueType))
        case SetType(elementType) =>
          print("SetType", List(elementType))
      }
    }

    def printExpr(expr: SmtExpr): Doc = {
      expr match {
        case node: SmtExprNode =>
          node match {
            case Equals(left, right) =>
              print("Equals", List(left, right))
            case Not(of) =>
              print("Not", List(of))
            case ApplyConstructor(dt, constructor, args) =>
              print("ApplyConstructor", List(dt, constructor, args))
            case ApplySelector(dt, constructor, variable, expr) =>
              print("ApplySelector", List(dt, constructor, variable, expr))
            case IfThenElse(cond, ifTrue, ifFalse) =>
              print("IfThenElse", List(cond, ifTrue, ifFalse))
            case ApplyTester(dt, constructor, expr) =>
              print("ApplyTester", List(dt, constructor, expr))
            case MapSelect(map, key) =>
              print("MapSelect", List(map, key))
            case ConstantMap(keyType, defaultValue) =>
              print("ConstantMap", List(keyType, defaultValue))
            case MapStore(map, key, newValue) =>
              print("MapStore", List(map, key, newValue))
            case SetSingleton(value) =>
              print("SetSingleton", List(value))
            case SetInsert(set, values) =>
              print("SetInsert", List(set, values))
            case Union(left, right) =>
              print("Union", List(left, right))
            case Member(value, set) =>
              print("Member", List(value, set))
            case QuantifierExpr(quantifier, variable, expr) =>
              print("QuantifierExpr", List(quantifier, variable, expr))
            case And(left, right) =>
              print("And", List(left, right))
            case Or(left, right) =>
              print("Or", List(left, right))
            case Implies(left, right) =>
              print("Implies", List(left, right))
            case IsSubsetOf(left, right) =>
              print("IsSubsetOf", List(left, right))
            case SetContains(element, set) =>
              print("SetContains", List(element, set))
            case Leq(left, right) =>
              print("Leq", List(left, right))
            case Lt(left, right) =>
              print("Lt", List(left, right))
          }
        case Variable(name, typ) =>
          printContext.addDefinition(name, print("Variable", List(name, typ)))
          name
        case Const(b) =>
          print("Const", List(b))
        case ConstI(i) =>
          print("ConstI", List(i))
        case EmptySet(valueType) =>
          print("EmptySet", List(valueType))
        case Distinct(elems) =>
          print("Distinct", List(elems))
        case OpaqueExpr(kind, expr) =>
          print("OpaqueExpr", List(kind, expr))

      }
    }

    printExpr(expr)
  }
}
