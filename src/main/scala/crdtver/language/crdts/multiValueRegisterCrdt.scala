package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

case class multiValueRegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    return "multiValueRegister"
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
    return List(
      Operation("assign", CrdtTypeDefinition.makeParams(typeArgs, "value"))
    )


  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
    return List(
      Query("get", List(), typeArgs.head),
      Query("getFirst", List(), typeArgs.head),
      Query("mv_contains", typeArgs, BoolType())
    )

  def numberTypes: Int =
    return 1

  override def numberInstances: Int =
    return 0

  def getValue(state: State): List[String] = {
    var latestAssign = List[CallInfo]()
    for (call <- state.calls.values) {
      val opName = call.operation.operationName
      if (opName == "assign") {
        if (latestAssign.isEmpty || latestAssign.head.callClock.happensBefore(call.callClock)) {
          latestAssign = List(call)
        } else if (!latestAssign.head.callClock.happensBefore(call.callClock) && !latestAssign.head.callClock.happensAfter(call.callClock)) {
          latestAssign = latestAssign :+ call
        }
      }
    }
    return latestAssign.map(_.operation.args.head).map(x => x.toString)
  }

  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = name match {
    case "get" =>
      val valueList = getValue(state)
      if (valueList == null) {
        return AnyValue("not initialized")
      } else {
        AnyValue(valueList)
      }
    case "getFirst" =>
      val valueList = getValue(state)
      if (valueList.isEmpty) {
        return AnyValue("not initialized")
      } else {
        val value = valueList.head
        return AnyValue(value)
      }
    case "mv_contains" =>
      val valueList = getValue(state)
      if (valueList.isEmpty) {
        return AnyValue("not initialized")
      } else if (valueList.contains(args.head.toString)) {
        return AnyValue(true)
      } else {
        val value = AnyValue(false)
        return value
      }
  }

  /**
    *
    * @return formula for the getFirst and mvcontains query of multivalue RegisterCrdt
    */

  override def queryDefinitions(crdtInstance: CrdtInstance): List[InQueryDecl] = {
    val c1 = varUse("c1")
    val c2 = varUse("c2")
    val anyArgs = varUse("anyArgs")
    val callId1 = getVariable("c1", CallIdType())
    val callId2 = getVariable("c2", CallIdType())
    val any = getVariable("anyArgs", crdtInstance.typeArgs.head)
    val result = varUse("result")
    val args = varUse("args")
    List(InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "getFirst"),
      params = List[InVariable](),
      returnType = crdtInstance.typeArgs.head,
      implementation = None,
      ensures = Some(
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("assign", result)),
          not(isExists(callId2, calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), functionCall("assign", result)), happensBefore(c1, c2))))))))),
      annotations = Set()
    ), InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "mv_contains"),
      params = List(getVariable("args", crdtInstance.typeArgs.head)),
      returnType = BoolType(),
      ensures = None,
      implementation = Some(
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("assign", args)),
          not(isExists(callId2, isExists(any, calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), functionCall("assign", anyArgs)), happensBefore(c1, c2)))))))))),
      annotations = Set()
    )
    )
  }
}
