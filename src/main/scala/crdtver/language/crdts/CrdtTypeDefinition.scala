package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, InVariable, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{DependentReturnType, InQueryDecl, InTypeExpr, SimpleType, SourceTrace, TypeUnit}
import crdtver.language.crdts.AbstractMapCrdt.{DeleteAffectsBefore, DeleteAffectsBeforeAndConcurrent, DeleteAffectsNothing}
import crdtver.language.crdts.CrdtInstance.QuerySpecification
import crdtver.language.crdts.CrdtTypeDefinition.Operation
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}
import crdtver.utils.Result

import scala.collection.MapView
import scala.util.matching.Regex

object CrdtTypeDefinition {

  case class Operation(crdtInstance: CrdtInstance, name: UniqueName, params: List[Param], queryReturnType: InTypeExpr) {
    def isQuery: Boolean = queryReturnType != TypeUnit()

    def isMutator: Boolean = queryReturnType match {
      case _: TypeUnit => true
      case _: DependentReturnType => true
      case _ => false
    }

    def paramTypes: List[InTypeExpr] = params.map(_.typ)
  }

  def SimpleOperation(crdtInstance: CrdtInstance, name: UniqueName, params: List[Param], queryReturnType: InTypeExpr = TypeUnit()) =
    Operation(crdtInstance, name, params, queryReturnType)


  case class Param(name: UniqueName, typ: InTypeExpr)

  def param(name: String, typ: InTypeExpr)(implicit nameContext: NameContext) =
    Param(nameContext.newName(name), typ)


  def latestCalls(state: State): List[CallInfo] = {
    val res = for {
      (c1, ci1) <- state.calls.iterator
      if !state.calls.exists { case (c2, ci2) => c1 != c2 && ci1.happensBefore(ci2) }
    } yield ci1
    res.toList
  }

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(),
    SetCrdt("Set_g", SetCrdt.RemoveAffectsNothing()),
    SetCrdt("Set_rw", SetCrdt.RemoveAffectsBeforeAndConcurrent()),
    SetCrdt("Set_aw", SetCrdt.RemoveAffectsBefore()),
    multiValueRegisterCrdt(),
    CounterCrdt(),
    MapCrdt("Map_dw", hasDelete = true, deleteResets = DeleteAffectsBeforeAndConcurrent(), DeleteAffectsBeforeAndConcurrent()),
    MapCrdt("Map_uw", hasDelete = true, deleteResets = DeleteAffectsBefore(), DeleteAffectsBefore()),
    MapCrdt("Map_g", hasDelete = false, deleteResets = DeleteAffectsNothing(), DeleteAffectsNothing())
  )
}

abstract class CrdtInstance()(implicit val nameContext: NameContext) {

  def scope: String

  /** operations provided by this CRDT */
  def operations: List[CrdtTypeDefinition.Operation]

  /** evaluates a query (for the interpreter) */
  def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue

  def querySpecification(name: UniqueName, args: List[TypedAst.InExpr]): QuerySpecification


  def hasQuery(name: UniqueName): Boolean =
    operations.exists(op => op.isQuery && op.name == name)

  val updateDatatypeName: UniqueName = nameContext.newName(s"${scope}_update")

  val queryDatatypeName: UniqueName = nameContext.newName(s"${scope}_query")

  def updateDatatype: SimpleType = SimpleType(updateDatatypeName)()
  def queryDatatype: SimpleType = SimpleType(updateDatatypeName)()


  private def typeDeclFromOps(name: UniqueName, ops: List[Operation]): TypedAst.InTypeDecl =
        TypedAst.InTypeDecl(
          NoSource(),
          false,
          name,
          for (q <- ops) yield TypedAst.DataTypeCase(
            NoSource(),
            q.name,
            q.params.map(p => TypedAst.InVariable(NoSource(), p.name, p.typ)),
            TypedAst.SimpleType(name)(NoSource()))
        )

}

object CrdtInstance {
  def empty: CrdtInstance = new CrdtInstance()(new NameContext()) {
    override def scope: String = ""

    /** operations provided by this CRDT */
    override def operations: List[CrdtTypeDefinition.Operation] = List()

    /** evaluates a query (for the interpreter) */
    override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = ???

    override def querySpecification(name: UniqueName, args: List[TypedAst.InExpr]): QuerySpecification = ???

  }

  sealed abstract class QuerySpecification {

  }

  case class QueryImplementation(impl: TypedAst.InExpr) extends QuerySpecification

  /** creates a postcondition given the result variable */
  case class QueryPostcondition(postcondition: TypedAst.InExpr => TypedAst.InExpr) extends QuerySpecification

}


abstract class CrdtTypeDefinition {

  /** name of the CRDT */
  def name: String

  /** Creates a new instance of this CRDT class by giving the type arguments.
    * Returns a CrdtInstance on success and an error message otherwise */
  def makeInstance(scope: String, typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[CrdtInstance], crdtContext: NameContext): Result[CrdtInstance, String]

}

case class UniqueName(originalName: String, index: Int) {
  require(index >= 0)

  override def toString: String =
//    if (index == 0)
//      name
//    else
    originalName + "_" + index
}

object UniqueName {
  private val uniqueNamePattern: Regex = """([a-zA-Z0-9_]+)_([0-9]+)""".r

  def from(name: String): UniqueName = name match {
    case uniqueNamePattern(n, i) => UniqueName(n, i.toInt)
    case name => UniqueName(name, 0)
  }

}

class NameContext() {
  private var usedNames: Set[UniqueName] = Set()

  def newName(name: String): UniqueName = {
    var i = 0
    var res = UniqueName(name, i)
    while (usedNames contains res) {
      i += 1
      res = UniqueName(name, i)
    }
    usedNames += res
    res
  }
}