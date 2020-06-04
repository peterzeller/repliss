package crdtver.language.crdts.old

case class RegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    return "Register"
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
    return List(
      Operation("assign", CrdtTypeDefinition.makeParams(typeArgs, "value"))
    )


  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
    return List(
      Query("get", List(), typeArgs.head),
      Query("isEqualTo", CrdtTypeDefinition.makeParams(typeArgs, "other"), BoolType())
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
        val t = crdtinstance.typeArgs(0)
        val default = Interpreter.defaultValue(t, state)
        default
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
    val c3 = varUse("c3")
    val callId1 = getVariable("c1", CallIdType())
    val callId2 = getVariable("c2", CallIdType())
    val callId3 = getVariable("c3", CallIdType())
    val args = varUse("result")
    val resVar = getVariable("res", crdtInstance.typeArgs.head)
    val res = varUse("res")
    val valueVar = getVariable("value", crdtInstance.typeArgs.head)
    val value = varUse("value")
    val valueVar2 = getVariable("value2", crdtInstance.typeArgs.head)
    val value2 = varUse("value2")

    List(InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "get"),
      params = List[InVariable](),
      returnType = crdtInstance.typeArgs.head,
      implementation = None,
      ensures = Some(
        or(not(exists(List(callId3, valueVar2), calculateAnd(List(isVisible(c3), isEquals(getOp(c3), makeOperation("assign", value2)))))),
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), makeOperation("assign", args)),
          not(exists(List(callId2, valueVar), calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation("assign", value)), happensBeforeCall(c1, c2)))))))))),
      annotations = Set()
    ),
      InQueryDecl(
        source = NoSource(),
        name = Identifier(NoSource(), "isEqualTo"),
        params = List[InVariable](resVar),
        returnType = BoolType(),
        implementation = Some(
          isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), makeOperation("assign", res)),
            not(exists(List(callId2, valueVar), calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation("assign", value)), happensBeforeCall(c1, c2))))))))),
        ensures = None,
        annotations = Set()
      )
    )
  }
}
