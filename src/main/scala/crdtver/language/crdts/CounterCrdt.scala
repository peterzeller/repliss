package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAstHelper._
import crdtver.language.TypedAst.{CallIdType, IntType}
import crdtver.language.crdts.CrdtInstance.QueryPostcondition
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, SimpleOperation}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}
import crdtver.utils.{Err, Ok, Result}

case class CounterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    "Counter"
  }

  override def makeInstance(scope: String, typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], context: NameContext): Result[CrdtInstance, String] = {
    implicit val nameContext: NameContext = context
    if (typeArgs.nonEmpty || crdtArgs.nonEmpty) {
      return Err("Counters do not take type arguments")
    }
    Ok(new CrdtInstance {
      private val increment = nameContext.newName("increment")

      private val decrement = nameContext.newName("decrement")

      private val get = nameContext.newName("get")

      /** operations proviced by this CRDT */
      override def operations: List[Operation] =
        List(
          SimpleOperation(this, increment, List()),
          SimpleOperation(this, decrement, List()),
          SimpleOperation(this, get, List(), IntType()),
        )


      /** evaluates a query (for the interpreter) */
      override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = {
        if (name == get) {
          var res = 0
          for (call <- state.calls.values) {
            val opName = call.operation.operationName
            if (opName == increment) {
              res += 1
            } else if (opName == decrement) {
              res -= 1
            }
          }
          AnyValue(res)
        } else {
          throw new RuntimeException(s"unhandled query: $name")
        }
      }

      override def querySpecification(name: UniqueName, args: List[TypedAst.InExpr]): CrdtInstance.QuerySpecification = {
        if (name == get) {
          // TODO counter formula
          val callId1 = makeVariableU("c1", CallIdType())
          val callId2 = makeVariableU("c2", CallIdType())
          val c1 = varUse(callId1.name)
          val c2 = varUse(callId2.name)
          QueryPostcondition(resultVar => {
            isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(increment)),
              not(isExists(callId2, and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(increment)), happensBeforeCall(c1, c2))))))))
          })
        } else {
          ???
        }
      }

    })
  }


}
