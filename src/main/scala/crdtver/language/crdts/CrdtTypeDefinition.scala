package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, InTypeDecl}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAst.BoolType
import crdtver.language.crdts.AbstractMapCrdt.{DeleteAffectsBefore, DeleteAffectsBeforeAndConcurrent, DeleteAffectsNothing}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}
import crdtver.utils.Result

import scala.util.Try
import scala.util.matching.Regex

object CrdtTypeDefinition {

  sealed abstract class Operation {
    def isQuery: Boolean

    def name: UniqueName

    def params: List[Param]

    def paramTypes: List[InTypeExpr] = params.map(_.typ)
  }

  case class SimpleOperation(name: UniqueName, params: List[Param], queryReturnType: Option[InTypeExpr] = None) extends Operation {
    override def isQuery: Boolean = queryReturnType.isDefined
  }

  case class ComplexOperation(name: UniqueName, params: List[Param], nestedOperations: List[Operation]) extends Operation {
    override def isQuery: Boolean = false
  }


  case class Param(name: String, typ: InTypeExpr)


  def latestCalls(state: State): List[CallInfo] = {
    (for {
      (c1, ci1) <- state.calls
      if !state.calls.exists { case (c2, ci2) => c1 != c2 && ci1.happensBefore(ci2) }
    } yield ci1).toList
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

abstract class CrdtInstance {


  /** operations provided by this CRDT */
  def operations: List[CrdtTypeDefinition.Operation]

  /** evaluates a query (for the interpreter) */
  def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue

  /** returns the query definitions for this CRDT */
  def queryDefinitions: List[InQueryDecl]


  def hasQuery(name: String): Boolean =
    queryDefinitions.exists(_.name.name == name)
}

abstract class CrdtTypeDefinition {

  /** name of the CRDT */
  def name: String

  /** Creates a new instance of this CRDT class by giving the type arguments.
    * Returns a CrdtInstance on success and an error message otherwise */
  def makeInstance(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[CrdtInstance], crdtContext: CrdtContext): Result[CrdtInstance, String]

}

case class UniqueName(name: String, index: Int) {
  override def toString: String = name + "_" + index
}

object UniqueName {
  private val uniqueNamePattern: Regex = """([a-zA-Z0-9_]+)_([0-9]+)""".r

  def from(name: String): UniqueName = name match {
    case uniqueNamePattern(n, i) => UniqueName(n, i.toInt)
    case name => UniqueName(name, 0)
  }
  def from(id: Identifier): UniqueName = from(id.name)
}

class CrdtContext() {
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