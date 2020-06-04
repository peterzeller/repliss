package crdtver.language.crdts

import crdtver.language.InputAst.BuiltInFunc.BF_isVisible
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{ApplyBuiltin, BoolType, FunctionCall, TypeVarUse}
import crdtver.language.TypedAstHelper.{TypeExtensions, _}
import crdtver.language.crdts.FlagCrdt.Strategy
import crdtver.language.crdts.MapCrdt.MStrategy
import MapCrdt._

class MapCrdt(strategy: Strategy, deleteStrategy: MStrategy, val name: String) extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 1

  /** number of CRDT type parameters */
  override def numberInstances: Int = 1


  override def additionalDataTypes: List[TypedAst.InTypeDecl] = {
    List(
      dataType(MapOp, List(
        dtCase(RemoveKey, List("key" -> TypeVarUse("K")())),
        dtCase(NestedOp, List("key" -> TypeVarUse("K")(), "op" -> TypeVarUse("O")()))
      )),
      dataType(MapQuery, List(
        dtCase(ContainsKey, List("key" -> TypeVarUse("K")())),
        dtCase(NestedQuery, List("key" -> TypeVarUse("K")(), "q" -> TypeVarUse("Q")()))
      ))
    )
  }

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
    val K: TypedAst.InTypeExpr = typeArgs.head
    val V: ACrdtInstance = crdtArgs.head

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(MapOp, List(K, V.operationType))

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(MapQuery, List(K, V.queryType))

    override def queryReturnType(queryName: String, queryArgs: List[TypedAst.InExpr]): TypedAst.InTypeExpr = queryName match {
      case ContainsKey => BoolType()
      case NestedQuery =>
        queryArgs match {
          case List(_, FunctionCall(_, _, nestedQ, nestedArgs, _)) =>
            V.queryReturnType(nestedQ.name, nestedArgs)
        }
    }

    /** rewrites visibility condition with the visibility condition  */
    def rewriteVis(expr: TypedAst.InExpr, key: TypedAst.VarUse): TypedAst.InExpr = {
      expr.rewrite {
        case ApplyBuiltin(_, _, BF_isVisible(), List(c)) =>
          c.isVis && deleteStrategy.isActive(c, key)
      }
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = {
      val k = "k" :: new TypeExtensions(K)
      val n = "n" :: new TypeExtensions(V.operationType)
      List(
        queryDeclImpl(ContainsKey, List(k), BoolType(), strategy.impl(
          isEnable = c => exists(n, c.op === makeOperation(NestedOp, varUse(k), varUse(n))),
          isDisable = c => c.op === makeOperation(RemoveKey, varUse(k))
        ))
      ) ++ (for (nestedQry <- V.queryDefinitions()) yield {
        val qryName = s"NestedQuery_${nestedQry.name}"
        val k = uniqueName("k", nestedQry.params.map(_.name.name)) :: new TypeExtensions(K)
        val params = k :: nestedQry.params
        nestedQry.implementation match {
          case Some(impl) =>
            queryDeclImpl(qryName, params, nestedQry.returnType, rewriteVis(impl, varUse(k)))
          case None =>
            val ensures = nestedQry.ensures.get
            queryDeclEnsures(qryName, params, nestedQry.returnType, rewriteVis(ensures, varUse(k)))
        }
      })
    }

  }
}

object MapCrdt {

  private val MapOp = "MapOp"
  private val MapQuery = "MapQuery"
  private val RemoveKey = "RemoveKey"
  private val NestedOp = "NestedOp"
  private val NestedQuery = "NestedQuery"
  private val ContainsKey = "ContainsKey"

  sealed abstract class MStrategy {
    def isActive(c: TypedAst.InExpr, key: TypedAst.VarUse): TypedAst.InExpr = {
      val r = varUse("r")
      this match {
        case DeleteAffectsPrior() =>
          // there is no remove after c
          not(exists(r, r.op === makeOperation(RemoveKey, key) && r.isVis && c < r))
        case DeleteAffectsPriorAndConcurrent() =>
          // all removes are before c
          forall(r, (r.op === makeOperation(RemoveKey, key) && r.isVis) --> r < c)
      }
    }
  }

  case class DeleteAffectsPrior() extends MStrategy

  case class DeleteAffectsPriorAndConcurrent() extends MStrategy

}
