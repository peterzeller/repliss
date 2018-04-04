package repliss

import crdtver.Repliss
import crdtver.Repliss.{Quickcheck, ReplissResult, Result}
import crdtver.language.InputAst
import crdtver.language.InputAst.{IdType, InExpr}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, InvocationInfo, LocalState, SnapshotTime, State, TransactionId, WaitForNothing}
import crdtver.utils.Helper
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

//object LogicEvalTests extends Properties("") {
class RandomTestingTests extends FunSuite {

  def checkResource(name: String): Result[ReplissResult] = {
    val input = Helper.getResource(name)
    Repliss.checkInput(input, name, checks = List(Quickcheck()))
  }


  test("verify userbase example") {

    val res = checkResource("/examples/userbase.rpls")

    assert(!res.get().hasCounterexample)
  }
}
