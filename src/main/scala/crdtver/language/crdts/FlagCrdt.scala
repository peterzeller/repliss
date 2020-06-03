package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, DataTypeCase, InExpr, InTypeDecl, VarUse}
import crdtver.language.crdts.CrdtTypeDefinition._
import crdtver.language.crdts.FlagCrdt.Strategy
import crdtver.testing.Interpreter
import crdtver.language.TypedAstHelper._

class FlagCrdt(strategy: Strategy, val name: String) extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 0

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
    dataType(
      "FlagOp",
      List(
        dtCase("Enable", List()),
        dtCase("Disable", List())
      )
    ),
    dataType("FlagQuery", List(dtCase("ReadFlag", List())))
  )

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType("FlagOp")

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType("FlagQuery")

    override def queryReturnType(queryName: String, queryArgs: TypedAst.InExpr): TypedAst.InTypeExpr = queryName match {
      case "ReadFlag" => BoolType()
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = List(
      queryDeclImpl("ReadFlag", List(), BoolType(), strategy.impl(
        isEnable = c => c.op === makeOperation("Enable"),
        isDisable = c => c.op === makeOperation("Disable")
      ))
    )

  }
}

object FlagCrdt {

  sealed abstract class Strategy {


    def impl(isEnable: VarUse => InExpr, isDisable: VarUse => InExpr): TypedAst.InExpr = {
      val e = varUse("e")
      val d = varUse("e")
      this match {
        case EW() =>
          // there is an enable-op that has not been overridden by a disable
          exists(e, e.isVis && isEnable(e) && not(exists(d, d.isVis && isDisable(d) && e < d)))
        case SEW() =>
          // there is an enable-op for and there is no disable coming after all enables
          exists(e, e.isVis && isEnable(e)) &&
            not(exists(d, (d.isVis && isDisable(d)) && forall(e, (e.isVis && isEnable(e)) --> e < d)))
        case DW() =>
          // there is an enable-op and every disable has been overridden by an enable
          exists(e, e.isVis && isEnable(e)) &&
            forall(d, (d.isVis && isDisable(d)) --> exists(e, e.isVis && isEnable(e) && d < e))
        case SDW() =>
          // there is an enable-op and there is no disable coming after all enables
          exists(e, e.isVis && isEnable(e)) &&
            not(exists(d, d.isVis && isDisable(d) && forall(e, (e.isVis && isEnable(e)) --> e < d)))
      }
    }

  }

  case class EW() extends Strategy

  case class SEW() extends Strategy

  case class DW() extends Strategy

  case class SDW() extends Strategy

}