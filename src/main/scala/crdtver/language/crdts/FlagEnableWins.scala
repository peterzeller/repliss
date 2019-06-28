package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, SimpleOperation}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, State}
import crdtver.utils.{Err, Ok, Result}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag


case class FlagEnableWins(
) extends CrdtTypeDefinition {
  def name: String = {
    "Flag_ew"
  }

  override def makeInstance(typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], context: CrdtContext): Result[CrdtInstance, String] = {
    if (typeArgs.nonEmpty || crdtArgs.nonEmpty) {
      return Err("Counters do not take type arguments")
    }

    Ok(new CrdtInstance {
      private val enable = context.newName("enable")

      private val disable = context.newName("disable")

      private val value = context.newName("get")

      /** operations proviced by this CRDT */
      override def operations: List[Operation] =
        List(
          SimpleOperation(this, enable, List()),
          SimpleOperation(this, disable, List()),
          SimpleOperation(this, value, List(), BoolType())
        )

      /** evaluates a query (for the interpreter) */
      override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = name match {
        case value =>
          AnyValue(CrdtTypeDefinition.latestCalls(state).exists(ci => ci.operation.operationName == enable))
      }

      /** returns the query definitions for this CRDT */
      override def queryDefinitions: List[InQueryDecl] = {
        val c1 = varUse("c1")
        val c2 = varUse("c2")
        val callId1 = makeVariable("c1", CallIdType())
        val callId2 = makeVariable("c2", CallIdType())
        val args = varUse("args")
        List(InQueryDecl(
          source = NoSource(),
          name = Identifier(NoSource(), value.toString),
          params = List(),
          returnType = BoolType(),
          ensures = None,
          implementation = Some(
            isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(enable)),
              not(isExists(callId2, and(List(and(isVisible(c2), isEquals(getOp(c2), makeOperation(disable, args))), happensBeforeCall(c1, c2))))))))),
          annotations = Set()
        )
        )
      }
    })

  }

}
