package crdtver.language.crdts

import crdtver.language.TypedAst
import crdtver.language.TypedAst.TypeVarUse
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.ACrdtInstance.QueryStructure

class MVRegisterCrdt extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 1

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val RegisterOp = "RegisterOp"

  private val Assign = "Assign"

  private val MVRegisterQry = "MVRegisterQry"

  private val ReadFirst = "ReadFirst"

  private val MvContains = "MvContains"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
    dataType(
      RegisterOp,
      List("T"),
      List(
        dtCase(Assign, List("value" -> TypeVarUse("T")())),
      )
    ),
    dataType(MVRegisterQry, List("T"),List(
      dtCase(ReadFirst, List()),
      dtCase(MvContains, List("value" -> TypeVarUse("T")()))))
  )

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
    val T: TypedAst.InTypeExpr = typeArgs.head

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(RegisterOp, List(T))()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(MVRegisterQry, List(T))()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(ReadFirst, List()) => T
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = List(
      queryDeclEnsures(ReadFirst, List(), T, {
        val result = varUse("result", T)
        val c = varUse("c")
        val c2 = varUse("c2")
        val v = varUse("v", T)
        not(exists(c, exists(v, c.isVis && c.op === makeOperation(Assign, v)))) ||
          exists(c, c.isVis && c.op === makeOperation(Assign, result)
            && not(exists(c2, exists(v, c2.isVis && c < c2 && c2.op === makeOperation(Assign, v)))))
      }),
      {
        val x = "x" :: new TypeExtensions(T)
        queryDeclImpl(MvContains, List(x), T, {
          val c = varUse("c")
          val c2 = varUse("c2")
          val v = varUse("v", T)
          exists(c, c.isVis && c.op === makeOperation(Assign, varUse(x))
              && not(exists(c2, exists(v, c2.isVis && c < c2 && c2.op === makeOperation(Assign, v)))))
        })
      }
    )

    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] = MVRegisterCrdt.this.additionalDataTypes
  }

  /** name of the CRDT */
  override def name: String = "MultiValueRegister"
}

