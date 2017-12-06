package crdtver.language.crdts

import crdtver.language.ACrdtInstance
import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{BoolType, InQueryDecl, InTypeExpr}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, State}

object CrdtTypeDefinition {

  case class Operation(
    name: String,
    paramTypes: List[InTypeExpr]
  )

  case class Query(
    qname: String,
    qparamTypes: List[InTypeExpr],
    qreturnType: InTypeExpr
  )

  /**
    * Append the parameters of crdt inside nested maps.
    * Map[ColumnId, Set_aw[TaskId] ], the operation becomes add(ColumnId, TaskId)
    */

  def operation(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] = {
    var operation = List[Operation]()
    val instance = crdtArgs.head
    for (op <- instance.operations()) yield {
      val structname = op.name
      val structtype = typeArgs ++ op.paramTypes
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

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(), SetAdd(), SetRemove(), MapAddCrdt(), multiValueRegisterCrdt(), MapRemoveCrdt()
  )
}

abstract class CrdtTypeDefinition {

  def name: String

  def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Operation] = List()

  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Query]

  def numberTypes: Int

  def numberInstances: Int

  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue

  def queryDefinitions(crdtinstance: CrdtInstance): List[InQueryDecl]

}