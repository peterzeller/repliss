package crdtver.language.crdts

import crdtver.RunArgs
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst._
import crdtver.language.crdts.ACrdtInstance.QueryStructure
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, DataTypeValue, State}

import scala.util.{Failure, Success, Try}


abstract class ACrdtInstance {

  def operationType: TypedAst.InTypeExpr

  def queryType: TypedAst.InTypeExpr

  def queryReturnType(qry: QueryStructure): TypedAst.InTypeExpr

  def queryDefinitions(): List[InQueryDecl]

  /**
   * Evaluates a query efficiently in the interpreter.
   *
   * If the return value is None the interpreter will fall back to evaluating the specification.
   */
  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): Option[AnyValue] = None

  def additionalDataTypesRec: List[TypedAst.InTypeDecl]

}

object ACrdtInstance {

  case class QueryStructure(name: String, args: List[QueryStructure])

}

