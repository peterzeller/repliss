package crdtver.language

import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{ApplyBuiltin, BoolConst, FunctionCall, Identifier, InExpr, InQueryDecl, InTypeExpr, InVariable, IntConst, QuantifierExpr, VarUse}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}
import crdtver.language.InputAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag



abstract class ACrdtInstance {

  def hasQuery(queryName: String): Boolean = {
    // TODO make this more efficient
    val queries = this.queries()
    queries.exists(q => q.qname == queryName)
  }

  def operations(): List[CrdtTypeDefinition.Operation]

  def queries(): List[CrdtTypeDefinition.Query]

  def queryDefinitions(): List[InQueryDecl]

  /** Transforms crdt operations by filtering out the arguments according to map key.
    *
    * CrdtInstance example - Map[ColumnId, Set_aw[TaskId] ].
    * Sequence of operations : add(c1,t1), add(c2,t2), remove(c1,t2).
    * query - contains(c1,t1) transforms to ->
    *
    * {{{
    * add(t1), remove(t2).
    * query - contains(t1).}}}
    *
    * StructInstance example - {a: Counter, b: Set_aw[TaskId]}.
    * Sequence of operations : a_increment(), b_add(x), b_remove(x).
    * query - b_contains(x) transforms to ->
    *
    * {{{
    * add(x), remove(x).
    * query - contains(x).}}}
    */
  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue

}

object ACrdtInstance {


  def transformcrdt(name: String, args: List[AbstractAnyValue], state: State, acrdtinstance: ACrdtInstance): AnyValue = {
    acrdtinstance.evaluateQuery(name, args, state)
  }

  /**
    * @param definition - CrdtTypeDefintion: RegisterCrdt(), SetCrdt(), MapCrdt()
    * @param crdtArgs  - Nested MapCrdt(), empty for SetCrdt() and RegisterCrdt()
    */

  case class CrdtInstance(
    definition: CrdtTypeDefinition,
    typeArgs: List[TypedAst.InTypeExpr],
    crdtArgs: List[ACrdtInstance]
  ) extends ACrdtInstance {
    override def operations(): List[CrdtTypeDefinition.Operation] = {
      return definition.operations(typeArgs, crdtArgs)
    }

    override def queries(): List[CrdtTypeDefinition.Query] = {
      return definition.queries(typeArgs, crdtArgs)
    }

    override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = {
      return definition.evaluateQuery(name, args, state, this)
    }

    override def queryDefinitions(): List[InQueryDecl] = {
      return definition.queryDefinitions(this)
    }
  }

  case class StructInstance(
    fields: Map[String, ACrdtInstance]
  ) extends ACrdtInstance {

    /** Prefixes structinstance name to the operation name.
      *
      * name of structinstance : a
      * operations : add,remove,assign
      * updated operations : a_add, a_remove, a_assign
      *
      */

    override def operations(): List[CrdtTypeDefinition.Operation] = {
      for ((name, instance) <- fields.toList; op <- instance.operations()) yield {
        val opname = name + '_' + op.name
        op.copy(name = opname)
      }
    }

    /** Prefixes structinstance name to the query name.
      *
      * name of structinstance : a
      * queries : get, contains
      * updated queries : a_get, a_contains
      *
      */

    override def queries(): List[CrdtTypeDefinition.Query] = {
      for ((name, instance) <- fields.toList; q <- instance.queries()) yield {
        val opname = name + '_' + q.qname
        q.copy(qname = opname)
      }
    }

    override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = {
      var filtercalls = Map[CallId, Interpreter.CallInfo]()
      val (crdtname, instance) = fields.toList.find(f => name.startsWith(f._1)).get
      for (call <- state.calls.values) {
        var opName = call.operation.operationName
        if (opName.startsWith(crdtname + "_")) { // Operations that start with query struct instance
          opName = opName.replaceFirst(crdtname + "_", "")
          val opType = call.operation.args
          val opId = call.id
          filtercalls += (opId -> call.copy(operation = DataTypeValue(opName, opType)))
        }
      }
      val newState = state.copy(calls = filtercalls)
      val newName = name.replaceFirst(crdtname + "_", "")
      instance.evaluateQuery(newName, args, newState)
    }

    override def queryDefinitions(): List[InQueryDecl] = {
      var queryDeclList = List[InQueryDecl]()
      for ((name, crdtInstance) <- fields.toList) yield {
        for (eachQuery <- crdtInstance.queryDefinitions()) { // Multiple queries for each crdtInstance
          eachQuery.implementation match {
            case Some(x) =>
              val updatedExpr = updateExpr(x, name)
              val newQuery = eachQuery.copy(implementation = Some(updatedExpr),
                name = Identifier(NoSource(), name + "_" + eachQuery.name.name))
              queryDeclList = queryDeclList :+ newQuery
            case None =>
          }
          eachQuery.ensures match {
            case Some(x) =>
              val updatedExpr = updateExpr(x, name)
              val newQuery = eachQuery.copy(ensures = Some(updatedExpr),
                name = Identifier(NoSource(), name + "_" + eachQuery.name.name))
              queryDeclList = queryDeclList :+ newQuery
            case None =>
          }
        }
      }
      queryDeclList
    }

    private def updateExpr(x: InExpr, fName: String): InExpr = {
      x match {
        case v: VarUse =>
          v
        case b: BoolConst =>
          b
        case i: IntConst =>
          i
        case a: ApplyBuiltin => // Logical operators, Ex: a && b
          val updatedArgs = a.args.map(arg => updateExpr(arg, fName)) // call updateExpr on each expr. (updateExpr(a), updateExpr(b))
          a.copy(args = updatedArgs)
        case f: FunctionCall =>
          val newName = fName + '_' + f.functionName.name
          f.copy(functionName = Identifier(NoSource(), newName))
        case qe: QuantifierExpr =>
          val newExpr = updateExpr(qe.expr, fName)
          qe.copy(expr = newExpr)
      }
    }
  }

}

