package repliss

import crdtver.Repliss
import crdtver.language.InputAst
import crdtver.language.InputAst.{IdType, InExpr}
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, InvocationInfo, LocalState, SnapshotTime, State, TransactionId, WaitForNothing}
import crdtver.testing.{Interpreter, LogicEvaluatorConv}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

/**
  * To enable stacktraces run:
  * set testOptions  += Tests.Argument("-oF")
  *
  * To run single test:
  * testOnly *LogicEvalTests -- -z "happensBefore same"
  *
  */
class LogicEvalTests extends FunSuite with PropertyChecks {


  private val input = scala.io.Source.fromFile("src/main/resources/examples/userbase.rpls").mkString
  private val prog: InputAst.InProgram = Repliss.parseAndTypecheck("userbase", input).get()
  private val interpreter = new Interpreter(prog, 3)
  private val ac = interpreter.defaultAnyValueCreator

  implicit val generatorDrivenConf: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 5000)

  test("happensBefore same") {
    forAll { (state: State) =>
      val conv = new LogicEvaluatorConv(prog)
      val ls = LocalState(
                varValues = Map(),
                todo = List(),
                waitingFor = WaitForNothing(),
                currentTransaction = None,
                visibleCalls = Set()
              )
      val structure = new conv.DefineStructure(ls, state)

      for (c1 <- state.calls.keys; c2 <- state.calls.keys) {
        assert(interpreter.call_happensBefore(state, c1, c2)
          == structure.happensBeforeMap.get(c2).contains(c1),
          s"for calls $c1 and $c2\n with ${structure.happensBeforeMap}")
      }

      for (i1 <- state.invocations.keys; i2 <- state.invocations.keys) {
        assert(interpreter.invocation_happensBefore(state, i1, i2)
          == structure.invocationhappensBeforeMap.get(i2).contains(i1),
          s"for invoations $i1 and $i2\n with ${structure.invocationhappensBeforeMap}")
      }

    }
  }


  test("invariant 0") {
      forAll { (state: State) =>
        // (forall var r: invocationId, var g: invocationId, var u: UserId ::
        // ((((r.info == removeUser(u))
        // && (g.info == getUser(u)))
        // && (r happens before invocation g))
        // ==> (g.result == getUser_res(notFound()))))
        val expr: InExpr = prog.invariants(0).expr
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

    def genMapWrite: Gen[DataTypeValue] =
      for {
        u <- arbitrary[UserId]
        f <- oneOf("f_id", "f_name", "f_mail")
        w <- smallString
      } yield DataTypeValue("mapWrite", List(ac(u), ac(f), ac(w)))

    def genMapDelete: Gen[DataTypeValue] =
      for {
        u <- arbitrary[UserId]
      } yield DataTypeValue("mapDelete", List(ac(u)))

    for {
      id <- arbitrary[CallId]
      origin <- arbitrary[InvocationId]
      clock <- arbitrary[Set[CallId]]
      op <- oneOf(genMapWrite, genMapDelete)
    } yield CallInfo(
      id = id,
      operation = op,
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
