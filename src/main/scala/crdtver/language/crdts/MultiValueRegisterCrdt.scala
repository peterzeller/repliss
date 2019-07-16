package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

case class MultiValueRegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    return "MultiValueRegister"
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
    return List(
      Operation("assign", CrdtTypeDefinition.makeParams(typeArgs, "value"))
    )


  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
    return List(
      Query("get", List(), typeArgs.head),
      Query("getFirst", List(), typeArgs.head),
      Query("mv_contains", CrdtTypeDefinition.makeParams(typeArgs, "elem"), BoolType())
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

  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = {
    val valueList = getValue(state)
    name match {
      case "get" =>
        AnyValue(valueList)
      case "getFirst" =>
        if (valueList.isEmpty) {
          val t = crdtinstance.typeArgs(0)
          val default = Interpreter.defaultValue(t, state)
          println(s"Getting default value in MultiValueRegister for $t -> $default (${default.value.getClass})")
          default
        } else {
          val value = valueList.head
          AnyValue(value)
        }
      case "mv_contains" =>
        AnyValue(valueList.contains(args.head.toString))
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
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), makeOperation("assign", result)),
          not(isExists(callId2, calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation("assign", result)), happensBeforeCall(c1, c2))))))))),
      annotations = Set()
    ), InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "mv_contains"),
      params = List(getVariable("args", crdtInstance.typeArgs.head)),
      returnType = BoolType(),
      ensures = None,
      implementation = Some(
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), makeOperation("assign", args)),
          not(isExists(callId2, isExists(any, calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation("assign", anyArgs)), happensBeforeCall(c1, c2)))))))))),
      annotations = Set()
    )
    )
  }
}
