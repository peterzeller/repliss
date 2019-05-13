package crdtver.language.crdts

import crdtver.language.{ACrdtInstance, InputAst}
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{Identifier, InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAstHelper._
import crdtver.language.TypedAst.{CallIdType, IntType}
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, SimpleOperation}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

case class CounterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    "Counter"
  }

  override def makeInstance(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance], context: CrdtContext): Either[CrdtInstance, String] = {
    if (typeArgs.nonEmpty || crdtArgs.nonEmpty) {
      return Right("Counters do not take type arguments")
    }
    Left(new CrdtInstance {
      private val increment = context.newName("increment")

      private val decrement = context.newName("decrement")

      private val get = context.newName("get")

      /** operations proviced by this CRDT */
      override def operations: List[Operation] =
        List(
          SimpleOperation(increment, List()),
          SimpleOperation(decrement, List()),
          SimpleOperation(get, List(), Some(IntType())),
        )


      /** evaluates a query (for the interpreter) */
      override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = name match {
        case value =>
          var res = 0
          for (call <- state.calls.values) {
            val opName = call.operation.operationName
            if (opName == increment.toString) {
              res += 1
            } else if (opName == decrement.toString) {
              res -= 1
            }
          }
          AnyValue(res)
      }

      /** returns the query definitions for this CRDT */
      override def queryDefinitions: List[InQueryDecl] = {
        val c1 = varUse("c1")
        val c2 = varUse("c2")
        val callId1 = makeVariable("c1", CallIdType())
        val callId2 = makeVariable("c2", CallIdType())
        val args = varUse("result")
        List(InQueryDecl(
          source = NoSource(),
          name = Identifier(NoSource(), get.toString),
          params = List[InVariable](),
          returnType = IntType(),
          implementation = None,
          ensures = Some(
            // TODO counter formula
            isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(increment)),
              not(isExists(callId2, and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(increment)), happensBeforeCall(c1, c2))))))))),
          annotations = Set()
        )
        )
      }
    })
  }


}
