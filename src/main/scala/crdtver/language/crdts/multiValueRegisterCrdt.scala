package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Param, SimpleOperation}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}
import crdtver.utils.{Err, Ok, Result}

case class multiValueRegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    "multiValueRegister"
  }

  override def makeInstance(typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], ctxt: CrdtContext): Result[CrdtInstance, String] = (typeArgs, crdtArgs) match {
    case (List(elementType), List()) =>
      Ok(new CrdtInstance {
        private val assign = ctxt.newName("assign")

        private val get = ctxt.newName("get")

        private val contains = ctxt.newName("contains")

        private val getFirst = ctxt.newName("getFirst")

        /** operations provided by this CRDT */
        override def operations: List[Operation] = {
          List(
            SimpleOperation(assign, List(Param("value", elementType))),
            SimpleOperation(get, List(), Some(elementType)),
            SimpleOperation(getFirst, List(), Some(elementType)),
            SimpleOperation(contains, List(Param("elem", elementType)), Some(BoolType()))
          )
        }


        /** evaluates a query (for the interpreter) */
        override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = name match {
          case value =>
            val valueList = getValue(state)
            if (valueList == null) {
              AnyValue("not initialized")
            } else {
              AnyValue(valueList)
            }
          case value + "First" =>
            val valueList = getValue(state)
            if (valueList.isEmpty) {
              AnyValue("not initialized")
            } else {
              val value = valueList.head
              AnyValue(value)
            }
          case mv_contains =>
            val valueList = getValue(state)
            if (valueList.isEmpty) {
              AnyValue("not initialized")
            } else if (valueList.contains(args.head.toString)) {
              AnyValue(true)
            } else {
              val value = AnyValue(false)
              value
            }
        }

        /** returns the query definitions for this CRDT */
        override def queryDefinitions: List[InQueryDecl] = {
          val c1 = varUse("c1")
          val c2 = varUse("c2")
          val anyArgs = varUse("anyArgs")
          val callId1 = makeVariable("c1", CallIdType())
          val callId2 = makeVariable("c2", CallIdType())
          val any = makeVariable("anyArgs", elementType)
          val result = varUse("result")
          val args = varUse("args")
          List(InQueryDecl(
            source = NoSource(),
            name = Identifier(NoSource(), get + "First"),
            params = List[InVariable](),
            returnType = elementType,
            implementation = None,
            ensures = Some(
              isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(assign, result)),
                not(isExists(callId2, and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(assign, result)), happensBeforeCall(c1, c2))))))))),
            annotations = Set()
          ), InQueryDecl(
            source = NoSource(),
            name = Identifier(NoSource(), contains.name),
            params = List(makeVariable("args", elementType)),
            returnType = BoolType(),
            ensures = None,
            implementation = Some(
              isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(assign, args)),
                not(isExists(callId2, isExists(any, and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(assign, anyArgs)), happensBeforeCall(c1, c2)))))))))),
            annotations = Set()
          )
          )
        }

        def getValue(state: State): List[String] = {
          var latestAssign = List[CallInfo]()
          for (call <- state.calls.values) {
            val opName = call.operation.operationName
            if (opName == assign) {
              if (latestAssign.isEmpty || latestAssign.head.callClock.happensBefore(call.callClock)) {
                latestAssign = List(call)
              } else if (!latestAssign.head.callClock.happensBefore(call.callClock) && !latestAssign.head.callClock.happensAfter(call.callClock)) {
                latestAssign = latestAssign :+ call
              }
            }
          }
          latestAssign.map(_.operation.args.head).map(x => x.toString)
        }
      })
    case _ =>
      Err("Register needs one type argument")
  }

}
