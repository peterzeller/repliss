package crdtver.language.crdts

import crdtver.language.InputAst
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtInstance.{QueryImplementation, QueryPostcondition}
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Param, SimpleOperation}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}
import crdtver.utils.{Err, Ok, Result}

case class RegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    "Register"
  }

  override def makeInstance(scope: String, typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], ctxt: NameContext): Result[CrdtInstance, String] = (typeArgs, crdtArgs) match {
    case (List(elementType), List()) =>
      Ok(new CrdtInstance {
        private implicit val nameContext: NameContext = ctxt

        private val assign = ctxt.newName("assign")

        private val get = ctxt.newName("get")

        private val isEqualTo = ctxt.newName("isEqualTo")

        /** operations proviced by this CRDT */
        override def operations: List[Operation] =
          List(
            SimpleOperation(this, assign, List(Param("value", elementType))),
            SimpleOperation(this, get, List(), elementType),
            SimpleOperation(this, isEqualTo, List(Param("other", elementType)), BoolType())
          )

        /** evaluates a query (for the interpreter) */
        override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = {
          if (name == get) {
            var latestAssign: CallInfo = null
            for (call <- state.calls.values) {
              val opName = call.operation.operationName
              if (opName == assign) {
                if (latestAssign == null || latestAssign.callClock.happensBefore(call.callClock)) {
                  latestAssign = call
                }
              }
            }
            if (latestAssign == null) {
              Interpreter.defaultValue(elementType)
            } else {
              latestAssign.operation.args.head
            }
          } else if (name == isEqualTo) {
            AnyValue(evaluateQuery(get, args.init, state) == args.last)
          } else {
            throw new RuntimeException(s"unhandled query $name")
          }
        }

        /*
        /** returns the query definitions for this CRDT */
        override def queryDefinitions: List[InQueryDecl] = {
          val c1 = varUse("c1")
          val c2 = varUse("c2")
          val callId1 = makeVariable("c1", CallIdType())
          val callId2 = makeVariable("c2", CallIdType())
          val args = varUse("result")
          val resVar = makeVariable("res", elementType)
          val res = varUse("res")
          val valueVar = makeVariable("value", elementType)
          val value = varUse("value")

          List(InQueryDecl(
            source = NoSource(),
            name = Identifier(NoSource(), get.toString),
            params = List[InVariable](),
            returnType = elementType,
            implementation = None,
            ensures = Some(
              isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(assign, args)),
                not(exists(List(callId2, valueVar), and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(assign, value)), happensBeforeCall(c1, c2))))))))),
            annotations = Set()
          ),
            InQueryDecl(
              source = NoSource(),
              name = Identifier(NoSource(), isEqualTo.toString),
              params = List[InVariable](resVar),
              returnType = BoolType(),
              implementation = Some(
                isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(assign, res)),
                  not(exists(List(callId2, valueVar), and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(assign, value)), happensBeforeCall(c1, c2))))))))),
              ensures = None,
              annotations = Set()
            )
          )
        }

         */
        override def querySpecification(name: UniqueName, args: List[InExpr]): CrdtInstance.QuerySpecification = {
          if (name == isEqualTo) {
            val callId1 = makeVariableU("c1", CallIdType())
            val callId2 = makeVariableU("c2", CallIdType())
            val c1 = varUse(callId1.name)
            val c2 = varUse(callId2.name)
            val valueVar = makeVariableU("value", elementType)
            val value = varUse(valueVar.name)
            QueryPostcondition(res => {
              isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(assign, res)),
                not(exists(List(callId2, valueVar), and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(assign, value)), happensBeforeCall(c1, c2))))))))
            })
          } else {
            ???
          }
        }
      })
    case _ =>
      Err("Register needs one type argument")
  }


}
