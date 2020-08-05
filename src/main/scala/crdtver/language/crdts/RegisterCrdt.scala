package crdtver.language.crdts

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, TypeVarUse}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.ACrdtInstance.QueryStructure
import crdtver.language.crdts.FlagCrdt.Strategy

class RegisterCrdt extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 1

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val RegisterOp = "RegisterOp"

  private val Assign = "Assign"

  private val RegisterQry = "RegisterQry"

  private val ReadRegister = "ReadRegister"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
    dataType(
      RegisterOp,
      List("T"),
      List(
        dtCase(Assign, List("value" -> TypeVarUse("T")())),
      )
    ),
    dataType(RegisterQry, List("T"), List(dtCase(ReadRegister, List())))
  )

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
    val T: TypedAst.InTypeExpr = typeArgs.head

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(RegisterOp, List(T))()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(RegisterQry, List(T))()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(ReadRegister, List()) => T
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = List(
      queryDeclEnsures(ReadRegister, List(), T, {
        val result = varUse("result", T)
        val c = varUse("c")
        val c2 = varUse("c2")
        val v = varUse("v", T)
        not(exists(c, exists(v, c.isVis && c.op === makeOp(Assign, v)))) ||
          exists(c, c.isVis && c.op === makeOp(Assign, result)
            && not(exists(c2, exists(v, c2.isVis && c < c2 && c2.op === makeOp(Assign, v)))))
      })

    )

    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] = RegisterCrdt.this.additionalDataTypes
  }

  /** name of the CRDT */
  override def name: String = "Register"
}

