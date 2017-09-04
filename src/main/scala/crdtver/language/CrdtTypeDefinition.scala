package crdtver.language

import crdtver.language.InputAst.{BoolType, InTypeExpr}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, State}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

abstract class CrdtTypeDefinition {

  def name: String

  def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Operation] = List()

  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Query]

  def numberTypes: Int

  def numberInstances: Int

  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = ???


}

abstract class ACrdtInstance {

  def operations(): List[CrdtTypeDefinition.Operation]

  def queries(): List[CrdtTypeDefinition.Query]

}

object ACrdtInstance {

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
  }

  case class StructInstance(
    fields: Map[String, ACrdtInstance]
  ) extends ACrdtInstance {
    override def operations(): List[CrdtTypeDefinition.Operation] = {
      for ((name, instance) <- fields.toList; op <- instance.operations()) yield {
        val opname = name + '_' + op.name
        op.copy(name = opname)
      }
    }

    override def queries(): List[CrdtTypeDefinition.Query] = {
      for ((name, instance) <- fields.toList; q <- instance.queries()) yield {
        val opname = name + '_' + q.qname
        q.copy(qname = opname)
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


    override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = name match {
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

    def sortByPartialOrdering[T : ClassTag](list: List[T], lessThan: (T, T) => Boolean): List[T] = {
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
      if(ci1.happensBefore(ci2)) {
        return true
      } else if(ci1.happensAfter(ci2)){
        return false
      } else if (ci1.operation.operationName == "add" && ci1.operation.args == ci2.operation.args) {
        return false
      } else if (ci1.operation.operationName == "remove" && ci1.operation.args == ci2.operation.args) {
        return true
      }
      false
    }


    override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = name match {
      case "contains" =>
        var calls = List[(AnyValue, String, Interpreter.CallId )]()
        var callmap = Map[AnyValue, List[String]]()
        var variable = AnyValue()
        for (call <- state.calls.values) {
          val opName = call.operation.operationName
          val opType = call.operation.args.head
          val opId = call.id
          calls = calls :+ (opType, opName, opId)
        }
        calls = sortByPartialOrdering(calls, (c1: (AnyValue, String, Interpreter.CallId ), c2: (AnyValue, String, Interpreter.CallId )) =>  {
          val r = sorthappensbefore(state, c1._3, c2._3)
          println(s"$c1 <= $c2 : $r" )
          r
        })
        println(calls)
        callmap = calls.groupBy(_._1).mapValues(_.map(_._2))
        for ((k, v) <- callmap) {
          if (v.last == "remove" && k == args.head) {
            variable = AnyValue(false)
          } else if (v.last == "add" && k == args.head) {
            variable = AnyValue(true)
          }
        }
    return variable
    }
  }
  case class MapCrdt(
  ) extends CrdtTypeDefinition {
    def name: String = {
      return "Map"
    }

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
  }

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(), SetCrdt(), MapCrdt()
  )
}

