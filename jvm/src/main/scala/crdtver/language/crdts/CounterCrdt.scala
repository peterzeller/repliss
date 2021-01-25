package crdtver.language.crdts

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{AggregateExpr, BoolType, CallIdType, IntType}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.ACrdtInstance.{QueryStructure, printTypes}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, DataTypeValue}

class CounterCrdt extends CrdtTypeDefinition {
  /** name of the CRDT */
  override def name: String = "Counter"

  /** number of normal type parameters */
  override def numberTypes: Int = 0

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val CounterOp = "CounterOp"
  private val Increment = "Increment"
  private val CounterQry = "CounterQry"
  private val GetCount = "GetCount"
  override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
    dataType(
      CounterOp,
      List(),
      List(
        dtCase(Increment, List("amount" -> IntType()))
      )
    ),
    dataType(CounterQry, List(), List(dtCase(GetCount, List())))
  )

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {

    override def toString: String = s"${CounterCrdt.this.name}${printTypes(typeArgs, crdtArgs)}"

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(CounterOp)()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(CounterQry)()

    override def queryReturnType(q: ACrdtInstance.QueryStructure): TypedAst.InTypeExpr = q match {
          case QueryStructure(GetCount, List()) => IntType()
        }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] =  List(
      {
        val c = "c" :: CallIdType()
        val amount = "amount" :: IntType()
        queryDeclImpl(GetCount, List(), IntType(), sum(List(c, amount),
          isVisible(varUse(c)) && varUse(c).op === makeOp(Increment, varUse(amount)),
          varUse(amount)
        ))
      }
    )

    override def evaluateQuery(name: String, args: List[Interpreter.AbstractAnyValue], state: Interpreter.State, interpreter: Interpreter): Option[Interpreter.AnyValue] = {
      name match {
        case GetCount =>
          assert(args.isEmpty)
          val res =
            state.calls.values
              .map(x => x.operation match {
                case DataTypeValue("Op", List(op)) =>
                  op.value match {
                    case DataTypeValue(Increment, List(x)) => x.value.asInstanceOf[Int]
                  }
              })
              .sum
          Some(AnyValue(res))
      }
    }


    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] =
      CounterCrdt.this.additionalDataTypes
  }
}
