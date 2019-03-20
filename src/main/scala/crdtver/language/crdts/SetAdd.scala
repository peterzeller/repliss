package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, State}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag


case class SetAdd(
) extends CrdtTypeDefinition {
  def name: String = {
    return "Set_aw"
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
    return List(
      Operation("add", CrdtTypeDefinition.makeParams(typeArgs, "elem")),
      Operation("remove", CrdtTypeDefinition.makeParams(typeArgs, "elem"))
    )

  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
    return List(
      Query("contains", CrdtTypeDefinition.makeParams(typeArgs, "elem"), BoolType())
    )

  def numberTypes: Int =
    return 1

  override def numberInstances: Int =
    return 0

  def sortByPartialOrdering[T: ClassTag](list: List[T], lessThan: (T, T) => Boolean): List[T] = {
    val ts = list.toArray
    val len = ts.size
    val visited = Array.fill[Boolean](len)(false)
    val postOrder = ListBuffer.empty[Int]

    def visit(n: Int): Unit = {
      visited(n) = true
      for (i <- 0 until len)
        if (!visited(i) && lessThan(ts(i), ts(n)))
          visit(i)
      postOrder += n
    }

    for (i <- 0 until len)
      if (!visited(i))
        visit(i)

    assert(postOrder.size == len)

    postOrder.map(i => ts(i)).toList
  }

  def sorthappensbefore(state: State, c1: CallId, c2: CallId): Boolean = {
    val ci1 = state.calls(c1)
    val ci2 = state.calls(c2)
    if (ci1.happensBefore(ci2)) {
      return true
    } else if (ci1.happensAfter(ci2)) {
      return false
    } else if (ci1.operation.operationName == "add" && ci1.operation.args == ci2.operation.args) {
      return false // Case of concurrent operation - place remove before add for add wins semantics
    } else if (ci1.operation.operationName == "remove" && ci1.operation.args == ci2.operation.args) {
      return true
    }
    false
  }


  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = name match {
    case "contains" =>
      var calls = List[(AnyValue, String, Interpreter.CallId)]()
      for (call <- state.calls.values) {
        val opName = call.operation.operationName
        val opType = call.operation.args.head
        val opId = call.id
        if (opType == args.head) {
          calls = calls :+ (opType, opName, opId)
        }
      }
      calls = sortByPartialOrdering(calls, (c1: (AnyValue, String, Interpreter.CallId), c2: (AnyValue, String, Interpreter.CallId)) => {
        val r = sorthappensbefore(state, c1._3, c2._3)
        r
      })
      calls.lastOption match {
        case Some((_, "remove", _)) =>
          return AnyValue(false)
        case Some((_, "add", _)) =>
          return AnyValue(true)
        case None =>
          return AnyValue(false)
        case _ =>
          throw new Exception("Expected add or remove, Found " + calls.lastOption)
      }
  }

  /**
    * contains query formula for adds wins semantics
    *
    * @param crdtInstance
    * @return
    */

  override def queryDefinitions(crdtInstance: CrdtInstance): List[InQueryDecl] = {
    val c1 = varUse("c1")
    val c2 = varUse("c2")
    val callId1 = getVariable("c1", CallIdType())
    val callId2 = getVariable("c2", CallIdType())
    val args = varUse("args")
    List(InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "contains"),
      params = List(getVariable("args", crdtInstance.typeArgs.head)),
      returnType = BoolType(),
      ensures = None,
      implementation = Some(
        isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), makeOperation("add", args)),
          not(isExists(callId2, calculateAnd(List(and(isVisible(c2), isEquals(getOp(c2), makeOperation("remove", args))), happensBeforeCall(c1, c2))))))))),
      annotations = Set()
    )
    )
  }
}
