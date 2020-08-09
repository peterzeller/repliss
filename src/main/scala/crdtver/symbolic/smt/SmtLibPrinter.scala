package crdtver.symbolic.smt

import crdtver.symbolic.smt.Smt._
import crdtver.utils.PrettyPrintDoc
import crdtver.utils.PrettyPrintDoc.Doc

/**
 * Prints in SmtLib format
 */
object SmtLibPrinter {


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
  def print(cs: List[NamedConstraint]): Doc = {

    import crdtver.utils.PrettyPrintDoc._

    val context = PrintContext()
    val docs = for (c <- cs) yield
      ";; " <> c.description </>
        //        ";; " <> SmtPrinter.printScala(c.constraint, SmtPrinter.PrintContext()).prettyStr(120).replace("\n", "\n;; ") </>
        "(assert" <+> nested(4, printExpr(c.constraint, context)) <> ")"

    sep(line, context.definitions.map(e => e._2)) </>
      sep(line, docs) </>
      "(check-sat)"

  }


  def printExpr(expr: SmtExpr, printContext: PrintContext): Doc = {
    import crdtver.utils.PrettyPrintDoc._


    def printPart(part: Any): Doc = part match {
      case t: Doc => t
      case e: SmtExpr => printExpr(e, printContext)
      case s: String => s
      case t: Type =>
        printType(t)
      case q: Quantifier => q.toString
      case l: List[t] =>
        sExprL(l)
      case DatatypeConstructor(name, args) =>
        sExprL(name :: args.map(a => sExprA(a.name, a.typ)))
      case a: Array[_] =>
        sExprL(a.toList)
      case b: Boolean => b.toString
      case b: BigInt => b.toString
      case b: Int => b.toString
      case _ =>
        throw new RuntimeException(s"unhandled case ${part.getClass}: $part")
    }

    def sExprA(parts: Any*): Doc =
      sExprL(parts.toList)

    def sExprL(parts: List[Any]): Doc =
      if (parts.isEmpty) "()"
      else group("(" <> printPart(parts.head) <> nested(2, line <> sep(line, parts.tail.map(printPart)) <> ")"))

    def sExpr(name: String, parts: List[Any]): Doc =
      sExprL(name :: parts)

    def printType(t: Smt.Type): Doc = {
      t match {
        case Sort(name) =>
          val tname = s"t_$name"
          printContext.addDefinition(tname, sExprA("declare-sort", tname, 0))
          tname
        case Datatype(name, constructors) =>
          val tname = s"t_$name"
          printContext.addDefinition(tname, sExprA("declare-datatype", tname, sExprL(constructors)))
          tname
        case IntegerType() =>
          "Int"
        case BoolType() =>
          "Bool"
        case ArrayType(keyType, valueType) =>
          sExprA("Array", keyType, valueType)
        case SetType(elementType) =>
          sExprA("Array", elementType, BoolType())
      }
    }

    def printExpr2(expr: SmtExpr): Doc = {
      expr match {
        case node: SmtExprNode =>
          node match {
            case Equals(left, right) =>
              sExpr("=", List(left, right))
            case Not(of) =>
              sExpr("not", List(of))
            case ApplyConstructor(dt, constructor, List()) =>
              constructor.name
            case ApplyConstructor(dt, constructor, args) =>
              sExprL(constructor.name :: args)
            case ApplySelector(dt, constructor, variable, expr) =>
              sExprA(variable.name, expr)
            case IfThenElse(cond, ifTrue, ifFalse) =>
              sExprA("if", cond, ifTrue, ifFalse)
            case ApplyTester(dt, constructor, expr) =>
              sExprA(s"is-${constructor.name}", expr)
            case MapSelect(map, key) =>
              sExprA("select", map, key)
            case ConstantMap(keyType, defaultValue) =>
              sExpr("ConstantMap", List(keyType, defaultValue))
            case MapStore(map, key, newValue) =>
              sExpr("store", List(map, key, newValue))
            case SetSingleton(value) =>
              sExpr("SetSingleton", List(value))
            case SetInsert(set, List()) =>
              printExpr2(set)
            case SetInsert(set, vs) =>
              sExprA("store", printExpr2(SetInsert(set, vs.tail)), vs.head, "true")
            case Union(left, right) =>
              sExprA(sExprA("_", "map", "or"), left, right)
            case QuantifierExpr(quantifier, variable, expr) =>
              val q = quantifier match {
                case Forall() => "forall"
                case Exists() => "exists"
              }
              sExprA(q, sExprA(sExprA(variable.name, variable.typ)), expr)
            case And(left, right) =>
              sExpr("and", List(left, right))
            case Or(left, right) =>
              sExpr("or", List(left, right))
            case Implies(left, right) =>
              sExpr("=>", List(left, right))
            case IsSubsetOf(left, right) =>
              sExpr("IsSubsetOf", List(left, right))
            case SetContains(element, set) =>
              sExprA("select", set, element)
            case Leq(left, right) =>
              sExpr("<=", List(left, right))
            case Lt(left, right) =>
              sExpr("<", List(left, right))
            case ApplyFunc(f, args) =>
              sExpr(f.name, args)
            case Distinct(elems) =>
              if (elems.size > 1)
                sExprL("distinct" :: elems)
              else "true"
          }
        case Variable(name, typ) =>
          printContext.addDefinition(name, sExpr("declare-fun", List(name, "()", typ)))
          name
        case Const(b) =>
          b.toString
        case ConstI(i) =>
          i.toString()
        case EmptySet(valueType) =>
          sExprA(sExprA("as", "const", SetType(valueType)), "false")
        case OpaqueExpr(kind, expr) =>
          sExpr("OpaqueExpr", List(kind, expr.toString))

      }
    }

    printExpr2(expr)
  }
}
