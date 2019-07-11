package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, CallIdType, InQueryDecl, InTypeExpr}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtInstance.QueryImplementation
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Param, SimpleOperation}
import crdtver.language.crdts.SetCrdt.{RemoveAffectsBefore, RemoveAffectsBeforeAndConcurrent, RemoveAffectsNothing, RemoveStrategy}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, State}
import crdtver.utils.{Err, Ok, Result}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

object SetCrdt {

  sealed abstract class RemoveStrategy

  case class RemoveAffectsNothing() extends RemoveStrategy

  case class RemoveAffectsBefore() extends RemoveStrategy

  case class RemoveAffectsBeforeAndConcurrent() extends RemoveStrategy

}


case class SetCrdt(
  name: String,
  strategy: RemoveStrategy
) extends CrdtTypeDefinition {


  override def makeInstance(scope: String, typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], crdtContext: NameContext): Result[CrdtInstance, String] = {

    implicit val nameContext: NameContext = crdtContext

    (typeArgs, crdtArgs) match {
      case (List(elementType), List()) =>
        Ok(new CrdtInstance {

          private val add = nameContext.newName("add")

          private val remove = nameContext.newName("remove")

          private val contains = nameContext.newName("contains")

          /** operations provided by this CRDT */
          override def operations: List[Operation] = List(
            SimpleOperation(this, add, List(Param("elem", elementType))),
            SimpleOperation(this, remove, List(Param("elem", elementType))),
            SimpleOperation(this, contains, List(Param("elem", elementType)), BoolType())
          )

          /** evaluates a query (for the interpreter) */
          override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = {
            assert(name == contains)
            val elem = args.head
            val adds =
              state.calls.values.filter(c => {
                val op = c.operation
                op.operationName == add && op.args.head == elem
              })
            lazy val removes =
              state.calls.values.filter(c => {
                val op = c.operation
                op.operationName == add && op.args.head == elem
              })
            val res: Boolean = strategy match {
              case RemoveAffectsBefore() =>
                adds.exists(a => removes.forall(r => r.happensBefore(a)))
              case RemoveAffectsBeforeAndConcurrent() =>
                adds.exists(a => !removes.exists(r => r.happensAfter(a)))
              case RemoveAffectsNothing() =>
                adds.nonEmpty
            }
            AnyValue(res)
          }

          override def querySpecification(name: UniqueName, args: List[TypedAst.InExpr]): CrdtInstance.QuerySpecification = {
            if (name == contains) {
              val callId1 = makeVariableU("c1", CallIdType())
              val callId2 = makeVariableU("c2", CallIdType())
              val c1 = varUse(callId1.name)
              val c2 = varUse(callId2.name)
              val elem = args(0)
              val implementation = strategy match {
                case RemoveAffectsBefore() =>
                  isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(add, elem)),
                    forall(callId2, implies(and(isVisible(c2), isEquals(getOp(c2), makeOperation(remove, elem))), happensBeforeCall(c2, c1))))))
                case RemoveAffectsBeforeAndConcurrent() =>
                  isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(add, elem)),
                    not(isExists(callId2, and(List(and(isVisible(c2), isEquals(getOp(c2), makeOperation(remove, elem))), happensBeforeCall(c1, c2))))))))
                case RemoveAffectsNothing() =>
                  isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(add, elem)))))
              }

              QueryImplementation(implementation)
            } else {
              ???
            }
          }
        })
      case _ =>
        Err("Set CRDT requires one type parameter.")
    }


  }


}
