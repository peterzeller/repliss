package crdtver

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

object WhyAst {


  sealed abstract class TraceInfo

  case class AstElementTraceInfo(source: InputAst.AstElem) extends TraceInfo

  case class EndAtomicTraceInfo(source: InputAst.AstElem) extends TraceInfo

  case class TextTraceInfo(text: String) extends TraceInfo


  def test(): Unit = {
    val term = FunctionCall("foo", List(FunctionCall("bar", List())))
    walk(term) {
      case FunctionCall(name, args) =>
        println(s"visit $name")
    }
  }

  import scala.reflect.runtime.universe._

  /**
    * Returns a map from formal parameter names to types, containing one
    * mapping for each constructor argument.  The resulting map (a ListMap)
    * preserves the order of the primary constructor's parameter list.
    */
  def caseClassParamsOf[T](typeTag: TypeTag[T]): ListMap[String, Type] = {
    val tpe = typeTag.tpe
    val constructorSymbol = tpe.decl(termNames.CONSTRUCTOR)
    val defaultConstructor =
      if (constructorSymbol.isMethod) constructorSymbol.asMethod
      else {
        val ctors = constructorSymbol.asTerm.alternatives
        ctors.map {
          _.asMethod
        }.find {
          _.isPrimaryConstructor
        }.get
      }

    ListMap[String, Type]() ++ defaultConstructor.paramLists.reduceLeft(_ ++ _).map {
      sym => sym.name.toString -> tpe.member(sym.name).asMethod.returnType
    }
  }

  def walk[T <: Element](elem: T)(f: PartialFunction[Element, Unit])(implicit tt: TypeTag[T]) = {
    val params = caseClassParamsOf(tt)
    println(s"params = $params")
  }

  sealed abstract class Element {
    var trace: TraceInfo = _


    def setTrace(trace: TraceInfo): this.type = {
      this.trace = trace
      this
    }


    //    def walk(): Unit = this match {
    //      case File(theories) => theories.foreach(_.walk())
    //      case Theory(name, labels, declarations) => declarations.foreach(_.walk())
    //      case d: MDecl => d match {
    //        case GlobalLet(name, funBody, labels, isGhost) => funBody.walk()
    //        case GlobalLetRec(recDefn) =>
    //        case GlobalVariable(name, typ, isGhost, labels) =>
    //        case AbstractFunction(isGhost, name, labels, params, returnType, specs) =>
    //        case ExceptionDecl(name, labels, typ) =>
    //        case d: Declaration => d match {
    //          case TypeDecls(decls) =>
    //          case ConstantDecl(name, labels, typ, value) =>
    //          case LogicDecls(decls) =>
    //          case InductiveDecls(isCoinductive, decls) =>
    //          case Axiom(name, formula) =>
    //          case Lemma(name, formula) =>
    //          case Goal(name, formula) =>
    //          case Import(isClone, impExp, name, as, substitutions) =>
    //          case Namespace(name, declarations) =>
    //        }
    //      }
    //      case t: TypeExpression => t match {
    //        case TypeSymbol(name, typeArgs) =>
    //        case TypeVariable(name) =>
    //        case TupleType(types) =>
    //      }
    //      case t: Term => t match {
    //        case IntConst(value) =>
    //        case RealConstant(value) =>
    //        case BoolConst(value) =>
    //        case Symbol(name) =>
    //        case FunctionCall(funcName, args) =>
    //        case ArrayLookup(arrayTerm, indexTerm) =>
    //        case ArrayUpdate(arrayTerm, indexTerm, newValue) =>
    //        case Conditional(condition, ifTrue, ifFalse) =>
    //        case LetTerm(pattern, value, body) =>
    //        case Sequence(terms) =>
    //        case Loop(invariants, variant, body) =>
    //        case While(condition, invariants, variant, body) =>
    //        case MatchTerm(terms, cases) =>
    //        case QuantifierTerm(quantifier, binders, body) =>
    //        case Tuple(values) =>
    //        case RecordTerm(fields) =>
    //        case FieldAccess(recordTerm, fieldName) =>
    //        case FieldAssignment(recordTerm, fieldName, newValue) =>
    //        case FieldUpdate(recordTerm, fieldUpdates) =>
    //        case CastTerm(term, typ) =>
    //        case LabeledTerm(label, term) =>
    //        case CodeMark(name) =>
    //        case Old(term) =>
    //        case _: Assertion =>
    //      }
    //      case TermCase(pattern, term) =>
    //      case TermField(fieldName, term) =>
    //    }

  }


  sealed abstract class Ident(name: String) {
    override def toString: String = name
  }

  // identifier starting with upper case
  case class UIdent(name: String) extends Ident(name) {
    if (!name.charAt(0).isUpper) {
      try {
        throw new RuntimeException(name + " must start with upper case letter")
      } catch {
        case e: RuntimeException => e.printStackTrace()
      }

    }
  }


  implicit def uIdent(name: String): UIdent = UIdent(name)


  // identifier starting with lower case
  case class LIdent(name: String) extends Ident(name) {
    //    if (name.charAt(0).isUpper) {
    //      try {
    //        throw new RuntimeException(name + " must start with lower case letter")
    //      } catch {
    //        case e: RuntimeException => e.printStackTrace()
    //      }
    //    }
  }

  implicit def lIdent(name: String): LIdent = LIdent(name)

  sealed abstract class Qualid(scope: List[UIdent], name: Ident) {
    override def toString: String = {
      if (scope.isEmpty) {
        name.toString
      } else {
        (scope.map(_.toString()) ++ List(name.toString())).reduce(_ + "." + _)
      }
    }
  }

  case class UQualid(scope: List[UIdent], name: UIdent) extends Qualid(scope, name)

  implicit def UQualid(name: String): UQualid = UQualid(List(), name)

  case class LQualid(scope: List[UIdent], name: LIdent) extends Qualid(scope, name)

  implicit def LQualid(name: String): LQualid = LQualid(List(), name)

  implicit def LQualid(name: LIdent): LQualid = LQualid(List(), name)

  case class TQualid(scope: List[Ident], name: UIdent)


  case class File(theories: List[Theory]) extends Element


  case class Theory(
    name: UIdent,
    labels: List[Label],
    declarations: List[Declaration]
  ) extends Element

  case class Module(
    name: UIdent,
    labels: List[Label],
    declarations: List[MDecl]
  )

  sealed abstract class MDecl extends Element

  case class GlobalLet(
    name: LIdent,
    funBody: FunBody,
    labels: List[Label] = List(),
    isGhost: Boolean = false
  ) extends MDecl


  case class GlobalLetRec(
    recDefn: List[FunDefn]
  ) extends MDecl

  // val ...
  case class GlobalVariable(
    name: LIdent,
    typ: TypeExpression,
    isGhost: Boolean = false,
    labels: List[Label] = List()
  ) extends MDecl

  // val ...
  case class AbstractFunction(
    isGhost: Boolean = false,
    name: LIdent,
    labels: List[Label] = List(),
    params: List[TypedParam],
    returnType: TypeExpression,
    specs: List[Spec]
  ) extends MDecl

  case class ExceptionDecl(
    name: LIdent,
    labels: List[Label],
    typ: Option[TypeExpression]
  ) extends MDecl

  // TODO add namespace?

  sealed abstract class Declaration extends MDecl

  case class TypeDecls(
    decls: List[TypeDecl]
  ) extends Declaration

  case class ConstantDecl(
    name: LIdent,
    labels: List[Label],
    typ: TypeExpression,
    value: Option[Term]
  ) extends Declaration

  case class LogicDecls(
    decls: List[LogicDecl]
  ) extends Declaration

  case class InductiveDecls(
    isCoinductive: Boolean,
    decls: List[InductiveDecl]
  ) extends Declaration

  case class Axiom(
    name: Ident,
    formula: Term
  ) extends Declaration

  case class Lemma(
    name: Ident,
    formula: Term
  ) extends Declaration

  case class Goal(
    name: Ident,
    formula: Term
  ) extends Declaration

  case class Import(
    isClone: Boolean,
    impExp: ImpExp,
    name: TQualid,
    as: Option[UIdent],
    substitutions: List[SubstElt]

  ) extends Declaration

  case class Namespace(
    name: UIdent,
    declarations: List[Declaration]
  ) extends Declaration


  sealed abstract class SubstElt

  // TODO add SubstElt cases


  sealed abstract class ImpExp

  case class ImpExpImport() extends ImpExp

  case class ImpExpExport() extends ImpExp

  case class ImpExpNone() extends ImpExp


  case class TypeDecl(
    name: LIdent,
    typeParameters: List[TypeParamDecl] = List(),
    definition: TypeDefn,
    labels: List[Label] = List()
  )

  sealed abstract class TypeDefn

  case class AbstractType() extends TypeDefn

  case class AliasType(
    alias: TypeExpression
  ) extends TypeDefn

  case class AlgebraicType(
    cases: List[TypeCase],
    invariants: List[Invariant] = List()
  ) extends TypeDefn

  case class TypeCase(
    name: UIdent,
    paramsTypes: List[TypedParam] = List(),
    labels: List[Label] = List()
  )

  case class RecordType(
    fields: List[RecordField],
    invariants: List[Invariant]
  ) extends TypeDefn

  case class RecordField(
    name: LIdent,
    labels: List[Label],
    typ: TypeExpression,
    isGhost: Boolean,
    isMutable: Boolean
  ) extends TypeDefn


  // TOdO


  case class LogicDecl(
    name: LIdent,
    labels: List[Label],
    typeParams: List[TypedParam],
    typ: TypeExpression,
    implementation: Term
  )

  case class TypeParamDecl(
    name: LIdent,
    labels: List[Label]
  )

  case class TypedParam(
    name: LIdent,
    typ: TypeExpression,
    isGhost: Boolean = false,
    labels: List[Label] = List()
  )


  // TODO


  case class InductiveDecl(
    name: LIdent,
    labels: List[Label],
    typeParams: List[TypedParam],
    cases: List[InductiveCase]
  )

  case class InductiveCase(
    name: Ident,
    labels: List[Label],
    formula: Term
  )


  sealed abstract class TypeExpression extends Element {
    def ::(name: String) = TypedParam(name, this)

    def ::(name: LIdent) = TypedParam(name, this)
  }


  case class TypeSymbol(
    name: LQualid,
    typeArgs: List[TypeExpression] = List()
  ) extends TypeExpression


  case class TypeVariable(
    name: LIdent
  ) extends TypeExpression

  case class TupleType(
    types: List[TypeExpression]
  ) extends TypeExpression

  def unitType() = TupleType(List())


  // TODO remove alias
  type Expr = Term

  sealed abstract class Term extends Element {
    def ==>(right: Term) = FunctionCall("==>", List(this, right))

    def &&(right: Term) = FunctionCall("&&", List(this, right))

    def +(right: Term) = FunctionCall("+", List(this, right))

    def ||(right: Term) = FunctionCall("||", List(this, right))

    def ===(right: Term) = FunctionCall("==", List(this, right))

    def !==(right: Term) = FunctionCall("!=", List(this, right))

    def <==>(right: Term) = FunctionCall("<==>", List(this, right))

    def unary_!() = FunctionCall("!", List(this))

    def >=(right: Term) = FunctionCall(">=", List(this, right))

    def >(right: Term) = FunctionCall(">", List(this, right))

    def <=(right: Term) = FunctionCall("<=", List(this, right))

    def <(right: Term) = FunctionCall("<", List(this, right))

    def get(indexes: Term*) = FunctionCall("get", List(this) ++ indexes)

    //Lookup(this, indexes.toList)
  }


  def Assignment(left: Term, right: Term) = FunctionCall(":=", List(left, right))

  def Lookup(map: Term, keys: Term*) = FunctionCall("Map.get", List(map, Tuple(keys.toList)))

  def Lookup(map: Term, keys: List[Term]) = FunctionCall("Map.get", List(map, Tuple(keys)))

  // TODO add elements from formulas (page 80)

  case class IntConst(value: BigInt) extends Term

  case class RealConstant(value: BigDecimal) extends Term

  case class BoolConst(value: Boolean) extends Term

  case class Symbol(name: LQualid) extends Term {
    def $(args: Term*) = FunctionCall(name, args.toList)
  }

  // TODO remove and replace with symbol
  def IdentifierExpr(name: LQualid) = Symbol(name)

  case class FunctionCall(
    funcName: LQualid,
    args: List[Term]
  ) extends Term

  case class ArrayLookup(
    arrayTerm: Term,
    indexTerm: Term
  ) extends Term

  case class ArrayUpdate(
    arrayTerm: Term,
    indexTerm: Term,
    newValue: Term
  ) extends Term

  case class Conditional(
    condition: Term,
    ifTrue: Term,
    ifFalse: Term
  ) extends Term

  case class LambdaAbstraction(
    params: List[TypedParam],
    specs: List[Spec],
    otherSpecs: List[Spec],
    body: Term
  ) extends Term

  case class FunBody(
    params: List[TypedParam],
    returnType: Option[TypeExpression],
    specs: List[Spec] = List(),
    otherSpecs: List[Spec] = List(),
    body: Term
  ) extends Element


  case class LetTerm(
    pattern: Pattern,
    value: Term,
    body: Term
  ) extends Term

  case class LetRec(
    definitions: List[FunDefn],
    body: Term
  ) extends Element

  case class FunDefn(
    name: LIdent,
    body: FunBody,
    isGhost: Boolean = false,
    labels: List[Label] = List()
  )

  case class Sequence(
    terms: List[Term]
  ) extends Term

  def makeBlockL(terms: List[Term]): Term = {
    Sequence(terms.flatMap(getTerms))
  }

  def makeBlock(terms: Term*): Term =
    makeBlockL(terms.toList)

  def getTerms(t: Term): List[Term] = t match {
    case Sequence(terms) => terms.flatMap(getTerms)
    case _ => List(t)
  }


  case class Loop(
    invariants: List[Invariant],
    variant: Option[Variant],
    body: Term
  ) extends Term

  case class While(
    condition: Term,
    invariants: List[Invariant],
    variant: Option[Variant],
    body: Term
  ) extends Term

  // TODO for-loop

  // TODO raise exceptions

  // TODO blackbox


  case class MatchTerm(
    terms: List[Term],
    cases: List[TermCase]
  ) extends Term

  case class TermCase(
    pattern: Pattern,
    term: Term
  ) extends Element


  case class QuantifierTerm(
    quantifier: Quantifier,
    binders: List[TypedParam],
    body: Term
  ) extends Term

  sealed abstract class Quantifier

  case class Forall() extends Quantifier

  case class Exists() extends Quantifier


  case class Tuple(
    values: List[Term]
  ) extends Term

  case class RecordTerm(
    fields: List[TermField]
  ) extends Term

  case class TermField(
    fieldName: LQualid,
    term: Term
  ) extends Element

  case class FieldAccess(
    recordTerm: Term,
    fieldName: LQualid
  ) extends Term

  case class FieldAssignment(
    recordTerm: Term,
    fieldName: LQualid,
    newValue: Term
  ) extends Term

  case class FieldUpdate(
    recordTerm: Term,
    fieldUpdates: List[TermField]
  ) extends Term

  case class CastTerm(
    term: Term,
    typ: TypeExpression
  ) extends Term

  case class LabeledTerm(
    label: Label,
    term: Term
  ) extends Term

  case class Label()

  case class CodeMark(
    name: UIdent
  ) extends Term

  case class Old(
    term: Term
  ) extends Term

  case class At(
    term: Term,
    programPoint: UIdent
  )

  // Program expressions (Fig 7.7):


  sealed abstract class Pattern

  case class OrPattern(
    patterns: List[Pattern]
  ) extends Pattern

  case class TuplePattern(
    patterns: List[Pattern]
  ) extends Pattern

  case class CatchAllPattern() extends Pattern

  case class VariablePattern(
    name: LIdent
  ) extends Pattern

  case class ConstructorPattern(
    constructorName: UIdent,
    args: List[Pattern]
  ) extends Pattern

  case class BindingPattern(
    pattern: Pattern,
    name: LIdent
  )


  // TODO


  // Figure 7.6: Specification clauses in programs

  sealed abstract class Spec

  case class Requires(
    formula: Term
  ) extends Spec

  case class Ensures(
    formula: Term
  ) extends Spec

  case class Returns(
    cases: List[FormulaCase]
  ) extends Spec

  case class FormulaCase(
    pattern: Pattern,
    formula: Term
  )

  case class Reads(
    terms: List[Term]
  ) extends Spec

  case class Writes(
    terms: List[Term]
  ) extends Spec


  case class RaisesName(
    raised: List[UQualid]
  ) extends Spec

  case class Raises(
    cases: List[RaisesCase]
  ) extends Spec

  case class RaisesCase(
    name: UQualid,
    pattern: Option[Pattern],
    formula: Term
  )

  case class Variant(
    variants: List[OneVariant]
  ) extends Spec

  case class OneVariant(
    term: Term,
    variantRel: Option[LQualid]
  )

  case class Invariant(
    formula: Term
  )

  sealed abstract class Assertion(formula: Term) extends Term

  case class Assert(
    formula: Term
  ) extends Assertion(formula)

  case class Assume(
    formula: Term
  ) extends Assertion(formula)

  case class Check(
    formula: Term
  ) extends Assertion(formula)


  // Helpers:

  def Forall(vars: List[TypedParam], body: Term): Term = {
    QuantifierTerm(Forall(), vars, body)
  }

  def Forall(v: TypedParam, body: Term): Term = {
    QuantifierTerm(Forall(), List(v), body)
  }

  def Exists(vars: List[TypedParam], body: Term): Term = {
    QuantifierTerm(Exists(), vars, body)
  }

  def Exists(v: TypedParam, body: Term): Term = {
    QuantifierTerm(Exists(), List(v), body)
  }

  implicit def string2Identifier(s: String): Symbol = Symbol(s)

}
