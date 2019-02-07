package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{ApplyBuiltin, BF_equals, BF_getOperation, BoolConst, BoolType, CallIdType, FunctionCall, Identifier, InExpr, InQueryDecl, InTypeExpr, NoSource, QuantifierExpr, VarUse}
import crdtver.language.InputAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query, operation, query}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}

case class MapRemoveCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    return "Map_rw"
  }

  /**
    * Append the parameters of crdt inside nested maps.
    * Map[ColumnId, Set_aw[TaskId] ], the operation becomes add(ColumnId, TaskId)
    */

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] = {
    operation(typeArgs, crdtArgs)
  }

  override def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] = {
    query(typeArgs, crdtArgs)
  }

  def numberTypes: Int =
    return 1

  override def numberInstances: Int =
    return 1

  def updateOperation(c: VarUse, key: VarUse, crdtInstance: CrdtInstance): InExpr = {
    val aCrdtInstance = crdtInstance.crdtArgs.head
    val args = varUse("args")
    var operationList = List[ApplyBuiltin]()
    for (op <- aCrdtInstance.operations()) {
      val argsVar = getVariable("args", op.paramTypes.head)
      operationList = operationList :+ and(isVisible(c), isExists(argsVar, isEquals(getOp(c), mfunctionCall(op.name.toString(), List(key, args)))))
    }
    calculateOr(operationList)
  }

  override def queryDefinitions(crdtinstance: CrdtInstance): List[InQueryDecl] = {
    var queryDeclList = List[InQueryDecl]()
    val c1 = varUse("c1")
    val c2 = varUse("c2")
    val callId1 = getVariable("c1", CallIdType())
    val callId2 = getVariable("c2", CallIdType())
    val key = varUse("key")
    val existsQuery = InQueryDecl(
      source = NoSource(),
      name = Identifier(NoSource(), "exists"),
      params = List(getVariable("key", crdtinstance.typeArgs.head)),
      returnType = BoolType(),
      ensures = None,
      implementation = Some(
        isExists(callId1, and(updateOperation(c1, key, crdtinstance),
          forall(callId2, implies(and(isVisible(c2), isEquals(getOp(c2), functionCall("delete", key))), happensBefore(c2, c1)))))),
      annotations = Set()
    )
    queryDeclList = queryDeclList :+ existsQuery
    val instance = crdtinstance.crdtArgs.head
    for (eachQuery <- instance.queryDefinitions()) { // the queryDefinition method of the CrdtArg//
      val updateList = getVariable("id", crdtinstance.typeArgs.head) +: eachQuery.params // Append the id of Mapcrdt
      eachQuery.implementation match {
        case Some(x) =>
          val updatedExpr = updateExpr(x)
          val newQuery = eachQuery.copy(implementation = Some(updatedExpr), params = updateList)
          queryDeclList = queryDeclList :+ newQuery
        case None =>
      }
      eachQuery.ensures match {
        case Some(x) =>
          val updatedExpr = updateExpr(x)
          val newQuery = eachQuery.copy(ensures = Some(updatedExpr), params = updateList)
          queryDeclList = queryDeclList :+ newQuery
        case None =>
      }
    }
    queryDeclList
  }

  def rfilterCalls(state: State, args: List[AbstractAnyValue]): Map[CallId, CallInfo] = {
    var filtercalls = Map[CallId, CallInfo]()
    for (call <- state.calls.values) {
      val opName = call.operation.operationName
      val crdtId = call.operation.args.head
      val opType = call.operation.args.tail
      val opId = call.id
      if (crdtId == args.head && opName != "delete") // checks operations with same crdt id
        filtercalls += (opId -> call.copy(operation = DataTypeValue(opName, opType)))
    }
    for (callEach <- state.calls.values) {
      if (callEach.operation.operationName == "delete") {
        filtercalls = filtercalls.filter { case (k, v) => v.happensAfter(callEach) ||
          args.head != callEach.operation.args.head
        }
      }
    }
    filtercalls
  }

  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = {
    var filtercalls: Map[CallId, CallInfo] = rfilterCalls(state, args)
    if (name == "exists") {
      if (filtercalls.isEmpty) {
        return AnyValue(false)
      }
      else
        return AnyValue(true)
    }
    else {
      val newState = state.copy(calls = filtercalls)
      val crdtType = crdtinstance.crdtArgs.head
      crdtType.evaluateQuery(name, args.tail, newState)
    }
  }

  private def updateExpr(x: InExpr): InExpr = {
    x match {
      case ApplyBuiltin(s, t, BF_equals(), List(
      ApplyBuiltin(s1, t1, BF_getOperation(), List(c1)),
      fc)) =>
        val newfc = updateExpr(fc)
        newfc match {
          case FunctionCall(s2, t2, f, args, kind) =>
            val d = varUse("d")
            val deleteId = getVariable("d", CallIdType())
            val newExpr = and(
              ApplyBuiltin(s, t, BF_equals(), List(
                ApplyBuiltin(s1, t1, BF_getOperation(), List(c1)),
                newfc)),
              forall(deleteId, implies(and(isVisible(d), isEquals(getOp(d), functionCall("delete", args.head))), happensBefore(d, c1))))
            newExpr
          case _ =>
            throw new RuntimeException(s"unhandled case $newfc")
        }
      case v: VarUse =>
        v
      case b: BoolConst =>
        b
      case a: ApplyBuiltin => // Logical operators, Ex: a && b
        val updatedArgs = a.args.map(arg => updateExpr(arg)) // call updateExpr on each expr. (updateExpr(a), updateExpr(b))
        a.copy(args = updatedArgs)
      case f: FunctionCall =>
        val id = varUse("id")
        val newArgs = id +: f.args
        f.copy(args = newArgs)
      case qe: QuantifierExpr =>
        val nextExpr = updateExpr(qe.expr)
        qe.copy(expr = nextExpr)
    }
  }
}
