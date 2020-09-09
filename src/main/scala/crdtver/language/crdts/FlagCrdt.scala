package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{BoolType, DataTypeCase, InExpr, InTypeDecl, VarUse}
import crdtver.language.crdts.CrdtTypeDefinition._
import crdtver.language.crdts.FlagCrdt.Strategy
import crdtver.testing.Interpreter
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.ACrdtInstance.{EvalQryCtxt, QueryStructure, printTypes}
import crdtver.symbolic.{OperationContext, SVal, SortBoolean, SortCallId, SortDatatype}

class FlagCrdt(strategy: Strategy, val name: String) extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 0

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val FlagOp = "FlagOp"

  private val Enable = "Enable"

  private val Disable = "Disable"

  private val FlagQuery = "FlagQuery"

  private val ReadFlag = "ReadFlag"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
    dataType(
      FlagOp,
      List(),
      List(
        dtCase(Enable, List()),
        dtCase(Disable, List())
      )
    ),
    dataType(FlagQuery, List(), List(dtCase(ReadFlag, List())))
  )

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {

    override def toString: String = s"${FlagCrdt.this.name}${printTypes(typeArgs, crdtArgs)}"

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(FlagOp)()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(FlagQuery)()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(ReadFlag, List()) => BoolType()
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = List(
      queryDeclImpl(ReadFlag, List(), BoolType(), strategy.impl(
        isEnable = c => c.op === makeOp(Enable),
        isDisable = c => c.op === makeOp(Disable)
      ))
    )

    override def additionalDataTypesRec: List[InTypeDecl] = FlagCrdt.this.additionalDataTypes

//    override def evaluateQuerySymbolic(name: String, args: List[SVal[_]], ctxt: ACrdtInstance.EvalQryCtxt): SVal[_] = {
//      name match {
//        case ReadFlag =>
//          assert(args.isEmpty)
//          strategy.implSymbolic(
//            makeOp(Enable),
//            makeOp(Disable),
//            ctxt
//          )
//      }
//    }
  }
}

object FlagCrdt {

  sealed abstract class Strategy {
    def implSymbolic(
      enableOp: SVal[SortDatatype],
      disableOp: SVal[SortDatatype],
      ctxt: ACrdtInstance.EvalQryCtxt): SVal[SortBoolean] = {
      import SVal._

      implicit val operationCtxt: OperationContext = ctxt.operationCtxt

      val e = "e" :: SortCallId()
      val d = "d" :: SortCallId()

      def vis(e: SVal[SortCallId]) =
        e.isVisible && ctxt.visibilityCheck(e)

      val op = ctxt.nestOperation

      def isEnable(e: SVal[SortCallId]) =
        e.op === op(enableOp)

      def isDisable(e: SVal[SortCallId]) =
        e.op === op(disableOp)

      this match {
        case EW() =>
          // there is an Enable-op that has not been overridden by a Disable
          exists(e, vis(e) &&
            isEnable(e) &&
            not(exists(d, vis(d) && isDisable(d) && e < d)))
        case SEW() =>
          // there is an Enable-op for and there is no Disable coming after all enables
          exists(e, vis(e) && isEnable(e)) &&
            not(exists(d, (vis(d) && isDisable(d)) && forall(e, (vis(e) && isEnable(e)) --> (e < d))))
        case DW() =>
          // there is an Enable-op and every Disable has been overridden by an Enable
          exists(e, vis(e) && isEnable(e)) &&
            forall(d, (vis(d) && isDisable(d)) --> exists(e, vis(e) && isEnable(e) && d < e))
        case SDW() =>
          // there is an Enable-op and there is no Disable coming after all enables
          exists(e, vis(e) && isEnable(e)) &&
            not(exists(d, vis(d) && isDisable(d) && forall(e, (vis(e) && isEnable(e)) --> (e < d))))
      }

    }


    def impl(isEnable: VarUse => InExpr, isDisable: VarUse => InExpr): TypedAst.InExpr = {
      val e = varUse("e")
      val d = varUse("d")
      this match {
        case EW() =>
          // there is an Enable-op that has not been overridden by a Disable
          exists(e, e.isVis && isEnable(e) && not(exists(d, d.isVis && isDisable(d) && e < d)))
        case SEW() =>
          // there is an Enable-op for and there is no Disable coming after all enables
          exists(e, e.isVis && isEnable(e)) &&
            not(exists(d, (d.isVis && isDisable(d)) && forall(e, (e.isVis && isEnable(e)) --> e < d)))
        case DW() =>
          // there is an Enable-op and every Disable has been overridden by an Enable
          exists(e, e.isVis && isEnable(e)) &&
            forall(d, (d.isVis && isDisable(d)) --> exists(e, e.isVis && isEnable(e) && d < e))
        case SDW() =>
          // there is an Enable-op and there is no Disable coming after all enables
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