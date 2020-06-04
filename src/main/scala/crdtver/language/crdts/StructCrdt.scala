package crdtver.language.crdts

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, FunctionCall, TypeVarUse}
import crdtver.language.TypedAstHelper.{TypeExtensions, _}
import crdtver.language.crdts.FlagCrdt.Strategy

class StructCrdt(name: String, fields: Map[String, ACrdtInstance]) extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 0

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val Op = s"${name}Op"
  private val Query = s"${name}Query"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = {
    List(
      dataType(
        Op,
        (for ((fieldName, fieldInstance) <- fields) yield
          dtCase(fieldName, List("nested" -> fieldInstance.operationType))
          ).toList
      ),
      dataType(
        Query,
        (for ((fieldName, fieldInstance) <- fields) yield
          dtCase(s"${fieldName}Qry", List("nested" -> fieldInstance.queryType))
          ).toList
      )
    )
  }

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(Op)

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(Query)

    override def queryReturnType(queryName: String, queryArgs: List[TypedAst.InExpr]): TypedAst.InTypeExpr = queryName match {
      case s"${field}Qry" =>
        val instance = fields(field)
        queryArgs match {
          case List(FunctionCall(_, _, nestedQ, nestedArgs, _)) =>
            instance.queryReturnType(nestedQ.name, nestedArgs)
        }
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = {
      val x = "x" :: new TypeExtensions(T)
      List(
        queryDeclImpl(Contains, List(x), BoolType(), strategy.impl(
          isEnable = c => c.op === makeOperation(Add, varUse(x)),
          isDisable = c => c.op === makeOperation(Remove, varUse(x))
        ))
      )
    }

  }
}

