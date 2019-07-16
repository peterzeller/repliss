package repliss

import crdtver.Repliss.{Quickcheck, ReplissResult}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, CallAction, InvariantCheck, InvariantViolationException, InvocationId, LocalAction, NewId, Return, StartTransaction, TransactionId}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.tagobjects.Slow
import org.scalatest.{FunSuite, Matchers}

/**
  * Tests for the random test generator
  */
class InterpreterTests extends FunSuite with Matchers {




  test("find chatapp example") {

    val prog = Repliss.parseAndTypecheck("chatapp", Helper.getResource("/examples/failsToVerify/chatapp_fail1.rpls")).get()
    val i = new Interpreter(prog, RunArgs(), domainSize = 3)
    var s = Interpreter.State(interpreter = Some(i))
    val i1 = InvocationId(1)
    val i2 = InvocationId(2)
    val i3 = InvocationId(3)
    val i4 = InvocationId(4)
    val i5 = InvocationId(5)
    val t1 = TransactionId(1)
    val t2 = TransactionId(2)
    val t3 = TransactionId(3)
    val t4 = TransactionId(4)
    s = i.executeAction(s, CallAction(i1, "sendMessage", List(AnyValue("UserId_1"), AnyValue("String_0")))).get
    s = i.executeAction(s, LocalAction(i1, StartTransaction(t1, Set()))).get
    s = i.executeAction(s, LocalAction(i1, NewId(1))).get
    s = i.executeAction(s, LocalAction(i1, Return())).get

    println(s"knownIds = ${s.knownIds}")

    for (k <- s.knownIds.values; kk <- k.keys) {
      println(s"Id = $kk (${kk.value.getClass})")
    }

    s = i.executeAction(s, CallAction(i2, "editMessage", List(AnyValue("MessageId_001"), AnyValue("String_1")))).get
    s = i.executeAction(s, LocalAction(i2, StartTransaction(t2, Set(t1)))).get
    s = i.executeAction(s, LocalAction(i2, Return())).get

    s = i.executeAction(s, CallAction(i3, "deleteMessage", List(AnyValue("MessageId_001")))).get
    s = i.executeAction(s, LocalAction(i3, StartTransaction(t3, Set(t1)))).get
    s = i.executeAction(s, LocalAction(i3, Return())).get

    s = i.executeAction(s, CallAction(i4, "getMessage", List(AnyValue("MessageId_001")))).get
    s = i.executeAction(s, LocalAction(i4, StartTransaction(t4, Set(t2, t3)))).get
    s = i.executeAction(s, LocalAction(i4, Return())).get

    s = i.executeAction(s, CallAction(i5, "getMessage", List(AnyValue("MessageId_001")))).get
    try {
      s = i.executeAction(s, InvariantCheck(i5)).get
      fail("Invariant-check should fail")
    } catch {
      case iv: InvariantViolationException =>
        println(iv)
        for (info <- iv.info)
          println(info)
    }

  }


}
