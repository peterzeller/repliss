package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Query, operation, query}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}


/**
  * Grow-only map
  */
case class MapGCrdt(
) extends CrdtTypeDefinition {
  def name: String = {
    return "Map_g"
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
      val argsVar = makeVariable("args", op.paramTypes.head)
      operationList = operationList :+ and(isVisible(c), isExists(argsVar, isEquals(getOp(c), makeOperationL(op.name.toString(), List(key, args)))))
    }
    calculateOr(operationList)
  }

  override def queryDefinitions(crdtinstance: CrdtInstance): List[InQueryDecl] = {
    var queryDeclList = List[InQueryDecl]()
    val instance = crdtinstance.crdtArgs.head
    for (eachQuery <- instance.queryDefinitions()) { // the queryDefinition method of the CrdtArg//
      val updateList = makeVariable("id", crdtinstance.typeArgs.head) +: eachQuery.params // Append the id of Mapcrdt
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

  def filterCalls(state: State, args: List[AbstractAnyValue]): Map[CallId, CallInfo] = {
    var filtercalls = Map[CallId, CallInfo]()
    for (call <- state.calls.values) {
      val opName = call.operation.operationName
      val crdtId = call.operation.args.head
      val opType = call.operation.args.tail
      val opId = call.id
      if (crdtId == args.head) // checks operations with same crdt id
        filtercalls += (opId -> call.copy(operation = DataTypeValue(opName, opType)))
    }
    filtercalls
  }

  override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] = {
    operation(CrdtTypeDefinition.makeParams(typeArgs, "elem"), crdtArgs)
  }

  override def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] = {
    query(typeArgs, crdtArgs)
  }

  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = {
    val filtercalls: Map[CallId, CallInfo] = filterCalls(state, args)
    val newState = state.copy(calls = filtercalls)
    val crdtType = crdtinstance.crdtArgs.head
    crdtType.evaluateQuery(name, args.tail, newState)
  }

  private def updateExpr(x: InExpr): InExpr = {
    x match {
      case i: IntConst =>
        i
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
      case _: InAllValidSnapshots =>
        ???
    }
  }
}

