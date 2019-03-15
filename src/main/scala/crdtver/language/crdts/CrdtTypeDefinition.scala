package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{BoolType, InQueryDecl, InTypeExpr, InVariable, NoSource}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

object CrdtTypeDefinition {

  case class Operation(
    name: String,
    params: List[Param]
  ) {
    def paramTypes: List[InTypeExpr] = params.map(_.typ)
  }

  case class Param(name: String, typ: InTypeExpr)

  case class Query(
    qname: String,
    qparamTypes: List[InTypeExpr],
    qreturnType: InTypeExpr
  )


  def makeParams(types: List[InTypeExpr], names: String*): List[Param] = {
    require(types.length == names.length, s"Unexpected number of types for $names: $types")
    names.zip(types).map { case (n, t) => Param(n, t) }.toList
  }

  /**
    * Append the parameters of crdt inside nested maps.
    * Map[ColumnId, Set_aw[TaskId] ], the operation becomes add(ColumnId, TaskId)
    */
  def operation(typeArgs: List[Param], crdtArgs: List[ACrdtInstance]): List[Operation] = {
    var operation = List[Operation]()
    val instance = crdtArgs.head
    for (op <- instance.operations()) yield {
      val structname = op.name
      val structtype: List[Param] = typeArgs ++ op.params
      operation = operation :+ Operation(structname, structtype)
    }
    val map_delete = Operation("delete", typeArgs)
    operation = operation :+ map_delete
    return operation
  }

  def query(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] = {
    var query = List[Query]()
    val instance = crdtArgs.head
    for (op <- instance.queries()) yield {
      val structname = op.qname
      val structtype = typeArgs ++ op.qparamTypes
      val returntype = op.qreturnType
      query = query :+ Query(structname, structtype, returntype)
    }
    query = query :+ Query("exists", typeArgs, BoolType())
    return query
  }

  def latestCalls(state: State): List[CallInfo] = {
    (for {
      (c1, ci1) <- state.calls
      if !state.calls.exists { case (c2, ci2) => c1 != c2 && ci1.happensBefore(ci2) }
    } yield ci1).toList
  }

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(), SetAdd(), SetRemove(), MapAddCrdt(), MapGCrdt(), multiValueRegisterCrdt(), MapRemoveCrdt(), CounterCrdt()
  )
}

abstract class CrdtTypeDefinition {

  /** name of the CRDT */
  def name: String

  /** operations provided by the CRDT for given type arguments and crdt arguments */
  def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Operation] = List()

  /** Queries provided by the CRDT */
  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Query]

  def numberTypes: Int

  def numberInstances: Int

  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue

  def queryDefinitions(crdtinstance: CrdtInstance): List[InQueryDecl]

}