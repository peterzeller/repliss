package crdtver.language

import crdtver.language.InputAst.{BoolType, InTypeExpr}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

abstract class CrdtTypeDefinition {

  def name: String

  def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance] ): List[CrdtTypeDefinition.Operation] = List()

  def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[CrdtTypeDefinition.Query]

  def numberTypes: Int

  def numberInstances: Int

  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = ???


}

abstract class ACrdtInstance {

  def operations() : List[CrdtTypeDefinition.Operation]

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

    override def operations(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance] ): List[Operation] =
      return List(
        Operation("assign", typeArgs)
      )


    def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance] ): List[Query] =
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

    def queries(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance] ): List[Query] =
      return List(
        Query("contains", typeArgs, BoolType())
      )

    def numberTypes: Int =
      return 1

    override def numberInstances: Int =
      return 0
  }

  case class MapCrdt(
  )  extends CrdtTypeDefinition {
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

