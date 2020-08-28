package crdtver.language.crdts

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, CallIdType, IntType, TypeVarUse}
import crdtver.language.TypedAstHelper.{TypeExtensions, _}
import crdtver.language.crdts.ACrdtInstance.{QueryStructure, printTypes}
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

  private val GetSize = "GetSize"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = {
    List(
      dataType(
        SetOp,
        List("T"),
        List(
          dtCase(Add, List("x" -> TypeVarUse("T")())),
          dtCase(Remove, List("x" -> TypeVarUse("T")()))
        )
      ),
      dataType(SetQuery, List("T"),
        List(
          dtCase(Contains, List("x" -> TypeVarUse("T")())),
          dtCase(GetSize, List())))
    )
  }

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
    val T: TypedAst.InTypeExpr = typeArgs.head

    override def toString: String = s"${SetCrdt.this.name}${printTypes(typeArgs, crdtArgs)}"


    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(SetOp, List(T))()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(SetQuery, List(T))()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(Contains, List(_)) => BoolType()
      case QueryStructure(GetSize, List()) => IntType()
    }


    override def queryDefinitions(): List[TypedAst.InQueryDecl] = {
      val x = "x" :: new TypeExtensions(T)
      val c = "c" :: CallIdType()
      List(
        queryDeclImpl(Contains, List(x), BoolType(), strategy.impl(
          isEnable = c => c.op === makeOp(Add, varUse(x)),
          isDisable = c => c.op === makeOp(Remove, varUse(x))
        )),
        queryDeclImpl(GetSize, List(), BoolType(), sum(List(x),
          strategy.impl(
            isEnable = c => c.op === makeOp(Add, varUse(x)),
            isDisable = c => c.op === makeOp(Remove, varUse(x))),
          intConst(1)))
      )
    }

    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] = SetCrdt.this.additionalDataTypes
  }
}

