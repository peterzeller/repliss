package crdtver.language.crdts

import crdtver.language.{ACrdtInstance, TypedAst}
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.InTypeDecl
import crdtver.language.TypedAst.{InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAst.BoolType
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

object CrdtTypeDefinition {

  sealed abstract class Operation {
    def isQuery: Boolean

    def name: String
    def params: List[Param]

    def paramTypes: List[InTypeExpr] = params.map(_.typ)
  }

  case class SimpleOperation(name: String, params: List[Param], queryReturnType: Option[InTypeExpr] = None()) extends Operation {
    override def isQuery: Boolean = queryReturnType.isDefined
  }

  case class ComplexOperation(name: String, params: List[Param], nestedOperations: List[Operation]) extends Operation {
    override def isQuery: Boolean = false
  }


  case class Param(name: String, typ: InTypeExpr)


  def makeParams(types: List[InTypeExpr], names: String*): List[Param] = {
    require(types.length == names.length, s"Unexpected number of types for $names: $types")
    names.zip(types).map { case (n, t) => Param(n, t) }.toList
  }


  def latestCalls(state: State): List[CallInfo] = {
    (for {
      (c1, ci1) <- state.calls
      if !state.calls.exists { case (c2, ci2) => c1 != c2 && ci1.happensBefore(ci2) }
    } yield ci1).toList
  }

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(), SetAdd(), SetRemove(), MapAddCrdt(), MapGCrdt(), multiValueRegisterCrdt(), MapRemoveCrdt(), CounterCrdt()
  )
}

abstract class CrdtInstance {
  /** operations proviced by this CRDT */
  def operations: List[CrdtTypeDefinition.Operation]

  /** additional type definitions introduced by this CRDT */
  def typeDeclarations: List[InTypeDecl]

  /** evaluates a query (for the interpreter) */
  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue

  /** returns the query definitions for this CRDT */
  def queryDefinitions: List[InQueryDecl]
}

abstract class CrdtTypeDefinition {

  /** name of the CRDT */
  def name: String

  /** Creates a new instance of this CRDT class by giving the type arguments.
    * Returns a CrdtInstance on success and an error message otherwise */
  def makeInstance(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): Either[CrdtInstance, String]

}