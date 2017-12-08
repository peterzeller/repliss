package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{CallIdType, Identifier, InQueryDecl, InTypeExpr, InVariable, IntType, NoSource}
import crdtver.language.InputAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

case class CounterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    return "Counter"
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
    return List(
      Operation("increment", List()),
      Operation("decrement", List())
    )


  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
    return List(
      Query("get", List(), IntType())
    )

  def numberTypes: Int =
    return 0

  override def numberInstances: Int =
    return 0


  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = name match {
    case "get" =>
      var res = 0
      for (call <- state.calls.values) {
        val opName = call.operation.operationName
        if (opName == "increment") {
          res += 1
        } else if (opName == "decrement") {
          res -= 1
        }
      }
      AnyValue(res)
  }

  /**
    *
    * @return formula for the get query of RegisterCrdt
    */

  override def queryDefinitions(crdtInstance: CrdtInstance): List[InQueryDecl] = {
    val c1 = varUse("c1")
    val c2 = varUse("c2")
    val callId1 = getVariable("c1", CallIdType())
    val callId2 = getVariable("c2", CallIdType())
    val args = varUse("result")
    List(InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "get"),
      params = List[InVariable](),
      returnType = IntType(),
      implementation = None,
      ensures = Some(
        // TODO counter formula
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("increment")),
          not(isExists(callId2, calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), functionCall("increment")), happensBefore(c1, c2))))))))),
      annotations = Set()
    )
    )
  }
}
