package crdtver.language

import crdtver.language.InputAst.{BoolType, InTypeExpr}

abstract class CrdtTypeDefinition {

  def name: String

  def operations(typeArgs: List[InTypeExpr] ): List[CrdtTypeDefinition.Operation]

  def queries(typeArgs: List[InTypeExpr]): List[CrdtTypeDefinition.Query]

  def numberTypes: Int

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

    override def operations(typeArgs: List[InTypeExpr]): List[Operation] =
      return List(
        Operation("assign", typeArgs)
      )


    def queries(typeArgs: List[InTypeExpr]): List[Query] =
      return List(
        Query("get", List(), typeArgs.head)
      )

    def numberTypes: Int =
      return 1
  }

  case class SetCrdt(
  ) extends CrdtTypeDefinition {
    def name: String = {
      return "Set_aw"
    }

    override def operations(typeArgs: List[InTypeExpr]): List[Operation] =
      return List(
        Operation("add", typeArgs),
        Operation("remove", typeArgs)
      )

    def queries(typeArgs: List[InTypeExpr]): List[Query] =
      return List(
        Query("contains", typeArgs, BoolType())
      )

    def numberTypes: Int =
      return 1
  }

  val crdts: List[CrdtTypeDefinition] = List(
    RegisterCrdt(), SetCrdt()
  )
}

