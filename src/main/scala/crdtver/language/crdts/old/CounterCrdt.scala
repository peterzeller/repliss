package crdtver.language.crdts.old

import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.{ACrdtInstance, CrdtTypeDefinition}
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, State}

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
        exists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), makeOperation("increment")),
          not(exists(callId2, calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation("increment")), happensBeforeCall(c1, c2))))))))),
      annotations = Set()
    )
    )
  }
}
