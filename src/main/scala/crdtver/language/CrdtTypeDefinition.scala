package crdtver.language

import crdtver.language.ACrdtInstance.CrdtInstance
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{ApplyBuiltin, BoolConst, FunctionCall, Identifier, InAllValidSnapshots, InExpr, InQueryDecl, InTypeExpr, InVariable, IntConst, QuantifierExpr, VarUse}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}
import crdtver.language.InputAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.ComplexOperation
import crdtver.language.crdts.{CrdtContext, CrdtInstance, CrdtTypeDefinition, UniqueName}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag


case class StructInstance(
  fields: Map[String, CrdtInstance],
  crdtContext: CrdtContext
) extends CrdtInstance {

  private val fieldNames: Map[String, UniqueName] =
    fields.keys.map(k => k -> crdtContext.newName(k)).toMap

  /** Prefixes structinstance name to the operation name.
    *
    * name of structinstance : a
    * operations : add,remove,assign
    * updated operations : a_add, a_remove, a_assign
    *
    */


  override def operations: List[CrdtTypeDefinition.Operation] = {
    for ((fieldName, nestedInstance) <- fields.toList) yield {
      ComplexOperation(fieldNames(fieldName), List(),
        nestedInstance.operations)
    }
  }


  override def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): AnyValue = {
    val filteredCalls = state.calls.filter { case (c, ci) =>
      ci.operation.operationName == name
    }.mapValues( ci => ci.copy(operation = ci.operation.args.head.asInstanceOf[DataTypeValue]))
    val filteredState = state.copy(calls = filteredCalls)
    val nestedOp = args.head.asInstanceOf[DataTypeValue]
    fields(name).evaluateQuery(nestedOp.operationName, nestedOp.args, filteredState)
  }

  override def queryDefinitions: List[InQueryDecl] = {
    for ((name, crdtInstance) <- fields.toList; nestedQuery <- crdtInstance.queryDefinitions) yield {
        nestedQuery.implementation match {
          case Some(x) =>
            val updatedExpr = updateExpr(x, name)
            nestedQuery.copy(implementation = Some(updatedExpr),
              name = Identifier(NoSource(), name + "_" + nestedQuery.name.name))
          case None =>
            nestedQuery.ensures match {
                      case Some(x) =>
                        val updatedExpr = updateExpr(x, name)
                        val newQuery = nestedQuery.copy(ensures = Some(updatedExpr),
                          name = Identifier(NoSource(), name + "_" + nestedQuery.name.name))
                        queryDeclList = queryDeclList :+ newQuery
                      case None =>
                    }
        }
      }
    }
  }

  private def updateExpr(x: InExpr, fName: String): InExpr = {
    x match {
      case v: VarUse =>
        v
      case b: BoolConst =>
        b
      case i: IntConst =>
        i
      case a: ApplyBuiltin => // Logical operators, Ex: a && b
        val updatedArgs = a.args.map(arg => updateExpr(arg, fName)) // call updateExpr on each expr. (updateExpr(a), updateExpr(b))
        a.copy(args = updatedArgs)
      case f: FunctionCall =>
        val newName = fName + '_' + f.functionName.name
        f.copy(functionName = Identifier(NoSource(), newName))
      case qe: QuantifierExpr =>
        val newExpr = updateExpr(qe.expr, fName)
        qe.copy(expr = newExpr)
      case qe: InAllValidSnapshots =>
        val newExpr = updateExpr(qe.expr, fName)
        qe.copy(expr = newExpr)
    }
  }
}


