package repliss

import crdtver.Repliss
import crdtver.language.InputAst
import crdtver.language.InputAst.{IdType, InExpr}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, InvocationInfo, LocalState, SnapshotTime, State, TransactionId, WaitForNothing}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

//object LogicEvalTests extends Properties("") {
class LogicEvalTests extends FunSuite with PropertyChecks {


  private val input = scala.io.Source.fromFile("src/main/resources/examples/userbase.rpls").mkString
  private val prog: InputAst.InProgram = Repliss.parseAndTypecheck("userbase", input).get()
  private val interpreter = new Interpreter(prog, 3)
  private val ac = interpreter.defaultAnyValueCreator

  implicit val generatorDrivenConf: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  test("invariant 1") {
    forAll { (state: State) =>
      // (forall var u: UserId, var i: invocationId ::
      // (((i.info == removeUser(u)) && (i.result != NoResult()))
      // ==> (exists var c: callId :: ((c.origin == i) && (c.op == mapDelete(u)))) ))
      val expr: InExpr = prog.invariants(1).expr
      val ls = LocalState(
        varValues = Map(),
        todo = List(),
        waitingFor = WaitForNothing(),
        currentTransaction = None,
        visibleCalls = Set()

      )
      /*
    operationMap = {call_4=mapWrite, UserId_002, f_mail, String_0, call_6=queryop_mapGet, UserId_001, f_name, invalid, call_7=queryop_mapGet, UserId_001, f_mail, invalid, call_1=mapWrite, UserId_001, f_name, String_1, call_3=mapWrite, UserId_002, f_name, String_1, call_5=queryop_mapExists, UserId_001, true, call_2=mapWrite, UserId_001, f_mail, String_0}
    [error] happensBeforeMap = {call_4=[call_1, call_2, call_3, call_4], call_6=[call_4, call_6, call_1, call_3, call_5, call_2], call_7=[call_4, call_6, call_7, call_1, call_3, call_5, call_2], call_1=[call_1], call_3=[call_1, call_2, call_3], call_5=[call_4, call_1, call_3, call_5, call_2], call_2=[call_1, call_2]}

     */
      //    val a = interpreter.defaultAnyValueCreator
      //    val String_0 = a("String_0")
      //    val String_1 = a("String_1")
      //    val UserId_001 = a("UserId_001")
      //    val UserId_002 = a("UserId_002")
      //
      //    def registerUser_res(u: AnyValue) = a(DataTypeValue("registerUser_res", List(u)))
      //    def getUser_res(u: AnyValue) = a(DataTypeValue("getUser_res", List(u)))
      //    val found = a("found")

      //    val state = State(
      //      invocations = Map(
      //        InvocationId(1) -> InvocationInfo(InvocationId(1), DataTypeValue("registerUser", List(String_1, String_0)),Some(registerUser_res(UserId_001))),
      //        InvocationId(2) -> InvocationInfo(InvocationId(2),DataTypeValue("registerUser", List(String_1, String_0)),Some(registerUser_res(UserId_002))),
      //        InvocationId(3) -> InvocationInfo(InvocationId(3),DataTypeValue("getUser", List(UserId_001)), Some(getUser_res(found))),
      //        InvocationId(4) -> InvocationInfo(InvocationId(4),DataTypeValue("removeUser", List(UserId_002)),None)),
      //      knownIds = Map(IdType("UserId")() -> Set(UserId_001, UserId_002))
      //    )
      val res = interpreter.evalExpr(expr, ls, state)(interpreter.tracingAnyValueCreator)
      true
    }
  }

  test("invariant 2") {
    forAll { (state: State) =>
      // !((exists var write: callId, var delete: callId, var u: UserId, var f: userRecordField, var v: String ::
      // (((write.op == mapWrite(u, f, v))
      // && (delete.op == mapDelete(u)))
      // && (delete happens before write))) )
      val expr: InExpr = prog.invariants(2).expr
      val ls = LocalState(
        varValues = Map(),
        todo = List(),
        waitingFor = WaitForNothing(),
        currentTransaction = None,
        visibleCalls = Set()

      )

      val res = interpreter.evalExpr(expr, ls, state)(interpreter.tracingAnyValueCreator)
      true
    }
  }

  implicit def arbitraryInvocationId: Arbitrary[InvocationId] = Arbitrary {
    for (i <- arbitrary[Int]) yield InvocationId(Math.abs(i) % 3)
  }

  implicit def arbitraryCallId: Arbitrary[CallId] = Arbitrary {
    for (i <- arbitrary[Int]) yield CallId(Math.abs(i) % 3)
  }

  case class UserId(id: Int)

  implicit def arbitraryUserId: Arbitrary[UserId] = Arbitrary {
    for (i <- arbitrary[Int]) yield UserId(Math.abs(i) % 3)
  }

  def smallString: Gen[String] = oneOf("a", "b", "c")

  implicit def arbitraryInvocationInfo: Arbitrary[InvocationInfo] = {

    def genRegisterUser: Gen[(DataTypeValue, Option[AnyValue])] = {
      for {
        name <- smallString
        mail <- smallString
        res <- arbitrary[Option[UserId]]
      } yield (DataTypeValue("registerUser", List(ac(name), ac(mail))),
        res.map(r => ac(DataTypeValue("registerUser_res", List(ac(r))))))
    }

    def genGetUser: Gen[(DataTypeValue, Option[AnyValue])] = {
      for {
        id <- arbitrary[UserId]
        res <- oneOf(None, Some("found"))
      } yield (DataTypeValue("getUser", List(ac(id))),
        res.map(r => ac(DataTypeValue("getUser_res", List(ac(r))))))
    }

    def genRemoveUser: Gen[(DataTypeValue, Option[AnyValue])] = {
      for {
        id <- arbitrary[UserId]
      } yield (DataTypeValue("removeUser", List(ac(id))),
        None)
    }

    val gen: Gen[InvocationInfo] = for {
      id <- arbitrary[InvocationId]
      (o, r) <- oneOf(genGetUser, genRegisterUser, genRemoveUser)
    } yield {
      InvocationInfo(id, o, r)
    }

    Arbitrary(gen)
  }

  implicit def arbitraryCallInfo: Arbitrary[CallInfo] = Arbitrary {
    for {
      id <- arbitrary[CallId]
      origin <- arbitrary[InvocationId]
      clock <- arbitrary[Set[CallId]]
    } yield CallInfo(
      id = id,
      operation = DataTypeValue("mapWrite", List()), // DataTypeValue,
      callClock = SnapshotTime(clock), // SnapshotTime,
      callTransaction = TransactionId(0),
      origin = origin
    )
  }

  implicit def arbitraryState: Arbitrary[State] =
    Arbitrary {
      for {
        knownUsers <- arbitrary[Set[UserId]]
        invocs <- arbitrary[List[InvocationInfo]]
        calls <- arbitrary[List[CallInfo]]
      } yield {


        State(
          invocations = invocs.groupBy(_.id).mapValues(_ (0)),
          calls = calls.groupBy(_.id).mapValues(_ (0)),
          //
          //        InvocationId(1) -> InvocationInfo(InvocationId(1), DataTypeValue("registerUser", List(String_1, String_0)),Some(registerUser_res(UserId_001))),
          //        InvocationId(2) -> InvocationInfo(InvocationId(2),DataTypeValue("registerUser", List(String_1, String_0)),Some(registerUser_res(UserId_002))),
          //        InvocationId(3) -> InvocationInfo(InvocationId(3),DataTypeValue("getUser", List(UserId_001)), Some(getUser_res(found))),
          //        InvocationId(4) -> InvocationInfo(InvocationId(4),DataTypeValue("removeUser", List(UserId_002)),None)
          knownIds = Map(IdType("UserId")() -> knownUsers.map(ac(_)))
        )
      }
    }

  implicit def shrinkState: Shrink[State] = Shrink { state =>
    shrink(state.knownIds).map(x => state.copy(knownIds = x))
      .append(shrink(state.invocations).map(x => state.copy(invocations = x)))
      .append(shrink(state.calls).map(x => state.copy(calls = x)))
      .append(shrink(state.transactions).map(x => state.copy(transactions = x)))
  }

}
