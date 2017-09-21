package crdtver.language

import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{BoolType, InQueryDecl, InTypeExpr}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

abstract class CrdtTypeDefinition {

  def name: String

  def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Operation] = List()

  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Query]

  def numberTypes: Int

  def numberInstances: Int

  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State, crdtinstance: CrdtInstance): AnyValue
}

abstract class ACrdtInstance {

  def hasQuery(queryName: String): Boolean = {
    // TODO make this more efficient
    val queries = this.queries()
    queries.exists(q => q.qname == queryName)
  }


  def operations(): List[CrdtTypeDefinition.Operation]

  def queries(): List[CrdtTypeDefinition.Query]

  def queryDefinitions(): List[InQueryDecl] = ???

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
      val newstate = state.copy(calls = filtercalls)
      val newname = name.replaceFirst(crdtname + "_", "")
      instance.evaluateQuery(newname, args, newstate)
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
  }

  case class SetCrdt(
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
        return false
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
      val newstate = state.copy(calls = filtercalls)
      val crdtType = crdtinstance.crdtArgs.head
      crdtType.evaluateQuery(name, args.tail, newstate)
    }
  }

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(), SetCrdt(), MapCrdt()
  )
}


