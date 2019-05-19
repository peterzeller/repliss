package crdtver.language

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst._
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}
import crdtver.language.InputAstHelper._
import crdtver.language.TypedAst.FunctionKind.FunctionKindDatatypeConstructor
import crdtver.language.crdts.CrdtTypeDefinition.ComplexOperation
import crdtver.language.crdts.{CrdtContext, CrdtInstance, CrdtTypeDefinition, UniqueName}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag


case class StructInstance(
  fields: Map[UniqueName, CrdtInstance],
  crdtContext: CrdtContext
) extends CrdtInstance {


  /** Prefixes structinstance name to the operation name.
    *
    * name of structinstance : a
    * operations : add,remove,assign
    * updated operations : a_add, a_remove, a_assign
    *
    */


  override def operations: List[CrdtTypeDefinition.Operation] = {
    for ((fieldName, nestedInstance) <- fields.toList) yield {
      ComplexOperation(fieldName, List(),
        nestedInstance.operations)
    }
  }


  override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = {
    val filteredCalls = state.calls.filter { case (c, ci) =>
      ci.operation.operationName == name
    }.mapValues(ci => ci.copy(operation = ci.operation.args.head.asInstanceOf[DataTypeValue]))
    val filteredState = state.copy(calls = filteredCalls)
    val nestedOp = args.head.asInstanceOf[DataTypeValue]
    fields(name).evaluateQuery(nestedOp.operationName, nestedOp.args, filteredState)
  }

  override def queryDefinitions: List[InQueryDecl] = {
    for ((name, crdtInstance) <- fields.toList; nestedQuery1 <- crdtInstance.queryDefinitions) yield {
      val nestedQuery = nestedQuery1.copy(
        name = Identifier(NoSource(), name + "_" + nestedQuery1.name.name)
      )
      nestedQuery.implementation match {
        case Some(x) =>
          val updatedExpr = rewriteQuery(x, name)
          nestedQuery.copy(
            implementation = Some(updatedExpr),
          )
        case None =>
          nestedQuery.ensures match {
            case Some(x) =>
              val updatedExpr = rewriteQuery(x, name)
              nestedQuery.copy(
                ensures = Some(updatedExpr),
              )
            case None =>
              nestedQuery
          }
      }
    }
  }

  private def rewriteQuery(x: InExpr, fName: UniqueName): InExpr = {
    x match {
      case v: VarUse =>
        v
      case b: BoolConst =>
        b
      case i: IntConst =>
        i
      case a: ApplyBuiltin => // Logical operators, Ex: a && b
        val updatedArgs = a.args.map(arg => rewriteQuery(arg, fName)) // call updateExpr on each expr. (updateExpr(a), updateExpr(b))
        a.copy(args = updatedArgs)
      case f: FunctionCall =>
        // nest
        FunctionCall(f.source, SimpleType(fName.toString)(), Identifier(NoSource(), fName.toString),
          List(f), FunctionKindDatatypeConstructor())
      case qe: QuantifierExpr =>
        val newExpr = rewriteQuery(qe.expr, fName)
        qe.copy(expr = newExpr)
      case qe: InAllValidSnapshots =>
        val newExpr = rewriteQuery(qe.expr, fName)
        qe.copy(expr = newExpr)
    }
  }
}


