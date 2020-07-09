package crdtver.language.crdts

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, TypeVarUse}
import crdtver.language.TypedAstHelper._
import crdtver.language.TypedAstHelper.TypeExtensions
import crdtver.language.crdts.FlagCrdt.Strategy

class SetCrdt(strategy: Strategy, val name: String) extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 1

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0


  private val SetOp = "SetOp"

  private val Add = "Add"

  private val Remove = "Remove"

  private val SetQuery = "SetQuery"

  private val Contains = "Contains"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = {
    List(
      dataType(
        SetOp,
        List(
          dtCase(Add, List("x" -> TypeVarUse("T")())),
          dtCase(Remove, List("x" -> TypeVarUse("T")()))
        )
      ),
      dataType(SetQuery, List(dtCase(Contains, List("x" -> TypeVarUse("T")()))))
    )
  }

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
    val T: TypedAst.InTypeExpr = typeArgs.head
    
    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(SetOp)()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(SetQuery)()

    override def queryReturnType(queryName: String, queryArgs: List[TypedAst.InExpr]): TypedAst.InTypeExpr = queryName match {
      case Contains => BoolType()
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

