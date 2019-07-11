package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, CallIdType, InQueryDecl, InTypeExpr, InVariable}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtInstance.{QueryImplementation, QueryPostcondition}
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, param, SimpleOperation}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}
import crdtver.utils.{Err, Ok, Result}

case class multiValueRegisterCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    "multiValueRegister"
  }

  override def makeInstance(scope1: String, typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], context: NameContext): Result[CrdtInstance, String] = {
    (typeArgs, crdtArgs) match {
      case (List(elementType), List()) =>
        Ok(new CrdtInstance()(context) {
          override def scope: String = scope1

          private val assign = nameContext.newName("assign")

          private val get = nameContext.newName("get")

          private val contains = nameContext.newName("contains")

          private val getFirst = nameContext.newName("getFirst")

          /** operations provided by this CRDT */
          override def operations: List[Operation] = {
            List(
              SimpleOperation(this, assign, List(param("value", elementType))),
              SimpleOperation(this, get, List(), elementType),
              SimpleOperation(this, getFirst, List(), elementType),
              SimpleOperation(this, contains, List(param("elem", elementType)), BoolType())
            )
          }


          /** evaluates a query (for the interpreter) */
          override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = {
            if (name == get) {
              val valueList = getValue(state)
              if (valueList == null) {
                AnyValue("not initialized")
              } else {
                AnyValue(valueList)
              }
            } else if (name == getFirst) {
              val valueList = getValue(state)
              if (valueList.isEmpty) {
                AnyValue("not initialized")
              } else {
                val value = valueList.head
                AnyValue(value)
              }
            } else if (name == contains) {
              val valueList = getValue(state)
              if (valueList.isEmpty) {
                AnyValue("not initialized")
              } else if (valueList.contains(args.head.toString)) {
                AnyValue(true)
              } else {
                val value = AnyValue(false)
                value
              }
            } else {
              throw new RuntimeException(s"unhandled case $name")
            }
          }

          /*
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
                  }

           */

          override def querySpecification(name: UniqueName, args: List[TypedAst.InExpr]): CrdtInstance.QuerySpecification = {

            val callId1 = makeVariableU("c1", CallIdType())
            val callId2 = makeVariableU("c2", CallIdType())
            val c1 = varUse(callId1.name)
            val c2 = varUse(callId2.name)

            if (name == contains) {
              val any = makeVariableU("anyArgs", elementType)
              val anyArgs = varUse(any.name)
              val elem = args(0)
              QueryImplementation(
                isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(assign, elem)),
                  not(isExists(callId2, isExists(any, and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(assign, anyArgs)), happensBeforeCall(c1, c2)))))))))
              )
            } else if (name == getFirst) {
              QueryPostcondition(result =>
                isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(assign, result)),
                  not(isExists(callId2, and(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), makeOperation(assign, result)), happensBeforeCall(c1, c2))))))))
              )
            } else if (name == get) {
              ???
            } else {
              ???
            }
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

}
