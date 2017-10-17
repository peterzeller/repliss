package crdtver.language

import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{ApplyBuiltin, BoolConst, BoolType, CallIdType, FunctionCall, Identifier, InExpr, InQueryDecl, InTypeExpr, InVariable, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, NoSource, OperationType, QuantifierExpr, SimpleType, SomeOperationType, UnknownType, UnresolvedType, VarUse}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}
import crdtver.language.InputAstHelper._

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

abstract class CrdtTypeDefinition {

  def name: String

  def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Operation] = List()

  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Query]

  def numberTypes: Int

  def numberInstances: Int

  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue

  def queryDefinitions(crdtinstance: CrdtInstance): List[InQueryDecl]

}

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
    * @param definiton - CrdtTypeDefintion: RegisterCrdt(), SetCrdt(), MapCrdt()
    * @param crdtArgs  - Nested MapCrdt(), empty for SetCrdt() and RegisterCrdt()
    */

  case class CrdtInstance(
    definiton: CrdtTypeDefinition,
    typeArgs: List[InTypeExpr],
    crdtArgs: List[ACrdtInstance]
  ) extends ACrdtInstance {
    override def operations(): List[CrdtTypeDefinition.Operation] = {
      return definiton.operations(typeArgs, crdtArgs)
    }

    override def queries(): List[CrdtTypeDefinition.Query] = {
      return definiton.queries(typeArgs, crdtArgs)
    }

    override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = {
      return definiton.evaluateQuery(name, args, state, this)
    }

    override def queryDefinitions(): List[InQueryDecl] = {
      return definiton.queryDefinitions(this)
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

  case class RegisterCrdt(
  ) extends CrdtTypeDefinition {
    def name: String = {
      return "Register"
    }

    override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
      return List(
        Operation("assign", typeArgs)
      )


    def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
      return List(
        Query("get", List(), typeArgs.head)
      )

    def numberTypes: Int =
      return 1

    override def numberInstances: Int =
      return 0


    override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = name match {
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
          return AnyValue("not initialized")
        } else {
          return latestAssign.operation.args.head
        }
    }

    /**
      *
      * @return formula for the get query of RegisterCrdt
      */

    override def queryDefinitions(crdtinstance: CrdtInstance): List[InQueryDecl] = {
      val c1 = varUse("c1")
      val c2 = varUse("c2")
      val callId1 = getVariable("c1", CallIdType())
      val callId2 = getVariable("c2", CallIdType())
      val args = varUse("result")
      List(InQueryDecl(
        source = NoSource(),
        name = Identifier(NoSource(), "get"),
        params = List[InVariable](),
        returnType = crdtinstance.typeArgs.head,
        implementation = None,
        ensures = Some(
          isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("assign", args)),
            not(isExists(callId2, calculateAnd(List(isVisible(c2), notEquals(c1, c2), isEquals(getOp(c2), functionCall("assign", args)), happensBefore(c1, c2))))))))),
        annotations = Set()
      )
      )
    }
  }

  case class SetRemove(
  ) extends CrdtTypeDefinition {
    def name: String = {
      return "Set_rw"
    }

    override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
      return List(
        Operation("add", typeArgs),
        Operation("remove", typeArgs)
      )

    def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
      return List(
        Query("contains", typeArgs, BoolType())
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
        return true // Case of concurrent operation - place add before remove for remove wins semantics
      } else if (ci1.operation.operationName == "remove" && ci1.operation.args == ci2.operation.args) {
        return false
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
      * contains query formula for remove wins semantics
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
          isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("add", args)),
            forall(callId2, and(isVisible(c2), implies(isEquals(getOp(c2), functionCall("remove", args)), happensBefore(c2, c1)))))))),
        annotations = Set()
      )
      )
    }
  }

  case class SetAdd(
  ) extends CrdtTypeDefinition {
    def name: String = {
      return "Set_aw"
    }

    override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] =
      return List(
        Operation("add", typeArgs),
        Operation("remove", typeArgs)
      )

    def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] =
      return List(
        Query("contains", typeArgs, BoolType())
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
          isExists(callId1, calculateAnd(List(isVisible(c1), isEquals(getOp(c1), functionCall("add", args)),
            not(isExists(callId2, calculateAnd(List(and(isVisible(c2), isEquals(getOp(c2), functionCall("remove", args))), happensBefore(c1, c2))))))))),
        annotations = Set()
      )
      )
    }
  }

  case class MapCrdt(
  ) extends CrdtTypeDefinition {
    def name: String = {
      return "Map"
    }

    /**
      * Append the parameters of crdt inside nested maps.
      * Map[ColumnId, Set_aw[TaskId] ], the operation becomes add(ColumnId, TaskId)
      */

    override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Operation] = {
      val instance = crdtArgs.head
      for (op <- instance.operations()) yield {
        val structname = op.name
        val structtype = typeArgs ++ op.paramTypes
        Operation(structname, structtype)
      }
    }

    override def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] = {
      val instance = crdtArgs.head
      for (op <- instance.queries()) yield {
        val structname = op.qname
        val structtype = typeArgs ++ op.qparamTypes
        val returntype = op.qreturnType
        Query(structname, structtype, returntype)
      }
    }

    def numberTypes: Int =
      return 1

    override def numberInstances: Int =
      return 1

    override def queryDefinitions(crdtinstance: CrdtInstance): List[InQueryDecl] = {
      var queryDeclList = List[InQueryDecl]()
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

    override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue = {
      var filtercalls = Map[CallId, Interpreter.CallInfo]()
      for (call <- state.calls.values) {
        val opName = call.operation.operationName
        val crdtId = call.operation.args.head
        val opType = call.operation.args.tail
        val opId = call.id
        if (crdtId == args.head) // checks operations with same crdt id
          filtercalls += (opId -> call.copy(operation = DataTypeValue(opName, opType)))
      }
      val newState = state.copy(calls = filtercalls)
      val crdtType = crdtinstance.crdtArgs.head
      crdtType.evaluateQuery(name, args.tail, newState)
    }

    private def updateExpr(x: InExpr): InExpr = {
      x match {
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
          val newExpr = updateExpr(qe.expr)
          qe.copy(expr = newExpr)
      }
    }
  }

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(), SetAdd(), SetRemove(), MapCrdt()
  )
}



