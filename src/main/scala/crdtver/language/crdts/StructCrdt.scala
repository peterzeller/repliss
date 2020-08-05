package crdtver.language.crdts

import crdtver.language.InputAst.BuiltInFunc.{BF_equals, BF_getOperation}
import crdtver.language.InputAst.Identifier
import crdtver.language.TypedAst
import crdtver.language.TypedAst.ApplyBuiltin
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.ACrdtInstance.{Func, QueryStructure}
import crdtver.language.crdts.MapCrdt.NestedOp
import crdtver.utils.MapUtils.MapExtensions

class StructCrdt(structName: String, fields: Map[String, ACrdtInstance]) extends CrdtTypeDefinition {

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
        List(),
        (for ((fieldName, fieldInstance) <- fields) yield
          dtCase(fieldName, List("nested" -> fieldInstance.operationType))
          ).toList
      ),
      dataType(
        Query,
        List(),
        (for ((fieldName, fieldInstance) <- fields) yield
          dtCase(s"${fieldName}Qry", List("nested" -> fieldInstance.queryType))
          ).toList
      )
    )
  }

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr] = List(), crdtArgs: List[ACrdtInstance] = List()): ACrdtInstance = new ACrdtInstance {

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(Op)()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(Query)()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(s"${field}Qry", List(nested)) =>
        val instance = fields(field)
        instance.queryReturnType(nested)
    }

    /** rewrites nested query operation  */
    def rewriteNestedQry(field: String)(expr: TypedAst.InExpr): TypedAst.InExpr = {
      expr.rewrite {
        case fc@TypedAst.FunctionCall(_, typ, Identifier(_, "Op"), List(), List(op), kind) =>
          fc.copy(args = List(makeOperation(field, op)))
      }
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = {
      // the queries in
      (for ((fieldName, fieldInstance) <- fields; fieldQry <- fieldInstance.queryDefinitions()) yield {
        fieldQry.rewrite(fieldName + "Qry_" + fieldQry.name.name, rewriteNestedQry(fieldName))

      }).toList
    }

    override def toFlatQuery[T](fc: T)(implicit s: ACrdtInstance.QueryStructureLike[T]): Option[ACrdtInstance.Func[T]] = s.structure(fc) match {
      case Some(Func(name, List(nestedQry))) =>
        val s"${fieldName}Qry" = name
        val fieldInstance = fields.getE(fieldName)
        for (n <- fieldInstance.toFlatQuery(nestedQry)) yield {
          Func(name + "_" + n.name, n.args)
        }
      case None => None
    }

    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] =
      additionalDataTypes ++ fields.values.flatMap(_.additionalDataTypesRec)
  }

  /** name of the CRDT */
  override def name: String = structName
}

