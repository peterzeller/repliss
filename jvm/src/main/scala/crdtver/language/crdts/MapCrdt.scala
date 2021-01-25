package crdtver.language.crdts

import crdtver.language.InputAst.BuiltInFunc.{BF_equals, BF_getOperation, BF_isVisible}
import crdtver.language.{TypedAst, TypedAstHelper}
import crdtver.language.TypedAst.{ApplyBuiltin, BoolType, FunctionCall, TypeVarUse}
import crdtver.language.TypedAstHelper.{TypeExtensions, _}
import crdtver.language.crdts.FlagCrdt.Strategy
import crdtver.language.crdts.MapCrdt.MStrategy
import MapCrdt._
import crdtver.language.InputAst.Identifier
import crdtver.language.crdts.ACrdtInstance.{Func, QueryStructure, printTypes}
import crdtver.testing.Interpreter

class MapCrdt(strategy: Strategy, deleteStrategy: MStrategy, val name: String) extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 1

  /** number of CRDT type parameters */
  override def numberInstances: Int = 1


  override def additionalDataTypes: List[TypedAst.InTypeDecl] = {
    List(
      dataType(MapOp,
        List("K", "O"),
        List(
        dtCase(DeleteKey, List("key" -> TypeVarUse("K")())),
        dtCase(NestedOp, List("key" -> TypeVarUse("K")(), "op" -> TypeVarUse("O")()))
      )),
      dataType(MapQuery, List("K", "Q"),  List(
        dtCase(ContainsKey, List("key" -> TypeVarUse("K")())),
        dtCase(NestedQuery, List("key" -> TypeVarUse("K")(), "q" -> TypeVarUse("Q")()))
      ))
    )
  }

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {
    val K: TypedAst.InTypeExpr = typeArgs.head
    val V: ACrdtInstance = crdtArgs.head

    override def toString: String = s"${MapCrdt.this.name}${printTypes(typeArgs, crdtArgs)}"

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(MapOp, List(K, V.operationType))()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(MapQuery, List(K, V.queryType))()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(ContainsKey, List(_)) => BoolType()
      case QueryStructure(NestedQuery, List(_, nested)) =>
        V.queryReturnType(nested)
    }

    /** rewrites visibility condition with the visibility condition  */
    def rewriteVis(expr: TypedAst.InExpr, key: TypedAst.VarUse): TypedAst.InExpr = {
      expr.rewrite {
        case ApplyBuiltin(_, _, BF_isVisible(), List(c)) =>
          c.isVis && deleteStrategy.isActive(c, key, this)
//        case ApplyBuiltin(_, _, BF_equals(), List(ApplyBuiltin(_, _, BF_getOperation(), List(c)), op)) =>
//          c.op === makeOperation(NestedOp, key, op)
        case fc@TypedAst.FunctionCall(_, typ, Identifier(_, "Op"), List(), List(op), kind) =>
          fc.copy(args = List(makeOperation(NestedOp, key, op)))
      }
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = {
      val k = "k" :: new TypeExtensions(K)
      val n = "n" :: new TypeExtensions(V.operationType)
      List(
        queryDeclImpl(ContainsKey, List(k), BoolType(), strategy.impl(
          isEnable = c => exists(n, c.op === makeOp(NestedOp, varUse(k), varUse(n))),
          isDisable = c => c.op === makeOp(DeleteKey, varUse(k))
        ))
      ) ++ (for (nestedQry <- V.queryDefinitions()) yield {
        val qryName = s"NestedQuery_${nestedQry.name}"
        val k = uniqueName("k", nestedQry.params.map(_.name.name)) :: new TypeExtensions(K)
        val params = k :: nestedQry.params
        nestedQry.rewrite(qryName, rewriteVis(_, varUse(k))).copy(
          params = params
        )
      })
    }

    override def evaluateQuery(name: String, args: List[Interpreter.AbstractAnyValue], state: Interpreter.State, interpreter: Interpreter): Option[Interpreter.AnyValue] = {
      // TODO
      None
    }

    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] = MapCrdt.this.additionalDataTypes ++ crdtArgs.flatMap(_.additionalDataTypesRec)

    override def toFlatQuery[T](fc: T)(implicit s: ACrdtInstance.QueryStructureLike[T]): Option[ACrdtInstance.Func[T]] = s.structure(fc) match {
          case Some(Func(NestedQuery, List(key, nestedQry))) =>
            for (n <- V.toFlatQuery(nestedQry)) yield {
              Func(s"NestedQuery_${n.name}", key :: n.args)
            }
          case r@Some(Func(ContainsKey, List(_))) =>
            r
          case other =>
            throw new Exception(s"Did not match: $other\nfor$fc")
        }
  }

}

object MapCrdt {

  private val MapOp = "MapOp"
  private val MapQuery = "MapQuery"
  private val DeleteKey = "DeleteKey"
  private val NestedOp = "NestedOp"
  private val NestedQuery = "NestedQuery"
  private val ContainsKey = "ContainsKey"

  sealed abstract class MStrategy() {
    def isActive(c: TypedAst.InExpr, key: TypedAst.VarUse, instance: ACrdtInstance): TypedAst.InExpr = {
      val r = varUse("r")
      this match {
        case DeleteAffectsPrior() =>
          // there is no remove after c
          not(exists(r, r.op === instance.makeOp(DeleteKey, key) && r.isVis && c < r))
        case DeleteAffectsPriorAndConcurrent() =>
          // all removes are before c
          forall(r, (r.op === instance.makeOp(DeleteKey, key) && r.isVis) --> (r < c))
      }
    }
  }

  case class DeleteAffectsPrior() extends MStrategy

  case class DeleteAffectsPriorAndConcurrent() extends MStrategy

}
