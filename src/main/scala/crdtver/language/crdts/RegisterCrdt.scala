package crdtver.language.crdts

import crdtver.language.{ACrdtInstance, InputAst}
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Param, SimpleOperation}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

case class RegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    "Register"
  }

  override def makeInstance(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): Either[CrdtInstance, String] = (typeArgs, crdtArgs) match {
    case (List(elementType), List()) =>
      Left(new CrdtInstance {
        /** operations proviced by this CRDT */
        override def operations: List[Operation] =
          List(
            SimpleOperation("assign", List(Param("value", elementType))),
            SimpleOperation("get", List(), Some(elementType)),
            SimpleOperation("isEqualTo", List(Param("other", elementType)), Some(BoolType()))
          )

        /** additional type definitions introduced by this CRDT */
        override def typeDeclarations: List[InputAst.InTypeDecl] =
          List()

        /** evaluates a query (for the interpreter) */
        override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = name match {
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
              Interpreter.defaultValue(elementType)
            } else {
              latestAssign.operation.args.head
            }
          case "isEqualTo" =>
            AnyValue(evaluateQuery("get", args.init, state) == args.last)
        }

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
            name = Identifier(NoSource(), "get"),
            params = List[InVariable](),
            returnType = elementType,
            implementation = None,
            ensures = Some(
              isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation("assign", args)),
                not(exists(List(callId2, valueVar), and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation("assign", value)), happensBeforeCall(c1, c2))))))))),
            annotations = Set()
          ),
            InQueryDecl(
              source = NoSource(),
              name = Identifier(NoSource(), "isEqualTo"),
              params = List[InVariable](resVar),
              returnType = BoolType(),
              implementation = Some(
                isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation("assign", res)),
                  not(exists(List(callId2, valueVar), and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation("assign", value)), happensBeforeCall(c1, c2))))))))),
              ensures = None,
              annotations = Set()
            )
          )
        }
      })
    case _ =>
      Right("Register needs one type argument")
  }


}
