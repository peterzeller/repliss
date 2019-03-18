package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, State}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag


case class FlagEnableWins(
) extends CrdtTypeDefinition {
  def name: String = {
    return "Flag_ew"
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
    return List(
      Operation("enable", List()),
      Operation("disable", List())
    )

  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
    return List(
      Query("get", List(), BoolType())
    )

  def numberTypes: Int =
    return 0

  override def numberInstances: Int =
    return 0



  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = name match {
    case "get" =>
      AnyValue(CrdtTypeDefinition.latestCalls(state).exists(ci => ci.operation.operationName == "enable"))
  }


  override def queryDefinitions(crdtInstance: CrdtInstance): List[InQueryDecl] = {
    val c1 = varUse("c1")
    val c2 = varUse("c2")
    val callId1 = getVariable("c1", CallIdType())
    val callId2 = getVariable("c2", CallIdType())
    val args = varUse("args")
    List(InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "get"),
      params = List(),
      returnType = BoolType(),
      ensures = None,
      implementation = Some(
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("enable")),
          not(isExists(callId2, calculateAnd(List(and(isVisible(c2), isEquals(getOp(c2), functionCall("disable", args))), happensBefore(c1, c2))))))))),
      annotations = Set()
    )
    )
  }
}
