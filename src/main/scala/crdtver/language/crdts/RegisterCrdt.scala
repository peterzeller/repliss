package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr, InVariable, NoSource}
import crdtver.language.InputAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

case class RegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    return "Register"
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
    return List(
      Operation("assign", typeArgs)
    )


  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
    return List(
      Query("get", List(), typeArgs.head),
      Query("isEqualTo", List(typeArgs.head), BoolType())
    )

  def numberTypes: Int =
    return 1

  override def numberInstances: Int =
    return 0


  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = name match {
    case "get" =>
      var latestAssign: CallInfo = null
      for (call <- state.calls.values) {
        val opName = call.operation.operationName
        if (opName == "assign") {
          if (latestAssign == null || latestAssign.callClock.happensBefore(call.callClock)) {
            latestAssign = call
          }
        }
      }
      if (latestAssign == null) {
        Interpreter.defaultValue(crdtinstance.typeArgs(0))
      } else {
        return latestAssign.operation.args.head
      }
    case "isEqualTo" =>
      AnyValue(evaluateQuery("get", args.init, state, crdtinstance) == args.last)
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
    val resVar = getVariable("res", crdtInstance.typeArgs.head)
    val res = varUse("res")
    val valueVar = getVariable("value", crdtInstance.typeArgs.head)
    val value = varUse("value")

    List(InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "get"),
      params = List[InVariable](),
      returnType = crdtInstance.typeArgs.head,
      implementation = None,
      ensures = Some(
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("assign", args)),
          not(exists(List(callId2, valueVar), calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), functionCall("assign", value)), happensBefore(c1, c2))))))))),
      annotations = Set()
    ),
      InQueryDecl(
        source = NoSource(),
        name = Identifier(NoSource(), "isEqualTo"),
        params = List[InVariable](resVar),
        returnType = BoolType(),
        implementation = Some(
          isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("assign", res)),
            not(exists(List(callId2, valueVar), calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), functionCall("assign", value)), happensBefore(c1, c2))))))))),
        ensures = None,
        annotations = Set()
      )
    )
  }
}
