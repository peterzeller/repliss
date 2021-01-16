package repliss

import crdtver.language.TypedAst.{CallIdType, SimpleType}
import crdtver.language.TypedAstHelper
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, CallAction, CallId, CallInfo, DataTypeValue, InvariantViolationException, InvocationId, LocalAction, LocalState, NewId, Return, SnapshotTime, StartTransaction, State, TransactionId, WaitForNothing}
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.{Helper, TimeTaker}
import crdtver.{Repliss, RunArgs}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Tests for the random test generator
 */
class InterpreterEvaluateTest extends AnyFunSuite with Matchers {


  val exampleProg: String =
    """
      |type String
      |type Either = Left(x: String) | Right(y: String)
      |
      |
      |
      |""".stripMargin


  test("quantifier optimization") {

    val prog = Repliss.parseAndTypecheck("chatapp", exampleProg).get()

    val i = new Interpreter(prog, RunArgs(), domainSize = 500)

    import crdtver.language.TypedAstHelper._
    val t_string = SimpleType("String")()
    val t_either = SimpleType("Either")()
    val x = "x" :: t_string
    val y = "y" :: t_string
    val c1 = "c1" :: CallIdType()
    val c2 = "c2" :: CallIdType()
    val expr =
      forallL(List(c1, x),
        (varUse(c1).op === makeOperationL("Left", t_either, List(), List(varUse(x))))
          --> existsL(List(c2, y), (varUse(c2).op === makeOperationL("Right", t_either, List(), List(varUse(y)))) && varUse(x) === varUse(y)))

    val visibleCalls = Set[CallId]()
    val ls = LocalState(None, varValues = Map(), todo = List(), waitingFor = WaitForNothing(), None, visibleCalls)
    val strings: Array[AnyValue] = i.enumerateValues(t_string, State()).toArray

    var id = 0
    def callInfo(name: String, arg: Int): CallInfo = {
      id += 1
      CallInfo(CallId(id), DataTypeValue("Op", List(AnyValue(DataTypeValue(name, List(strings(arg)))))), SnapshotTime(Set()), TransactionId(0), InvocationId(0))
    }

    val calls = List(
      callInfo( "Left", 3),
      callInfo("Right", 3),
      callInfo( "Left", 7),
      callInfo( "Left", 13),
      callInfo( "Left", 23),
      callInfo( "Left", 30),
      callInfo( "Left", 31),
      // xx
//      callInfo( "Left", 77),
      callInfo( "Left", 89),
      callInfo( "Right", 7),
      callInfo( "Right", 13),
      callInfo( "Right", 23),
      callInfo( "Right", 30),
      callInfo( "Right", 31),
      callInfo( "Right", 89),
      callInfo( "Right", 4),
      callInfo( "Right", 10),
      callInfo( "Right", 16),
      callInfo( "Right", 16),
      callInfo( "Right", 17),
      callInfo( "Right", 20),
      callInfo( "Right", 99)
    )
    val inState = State(
      calls = calls.map(x => x.id -> x).toMap
    )
    val (dur, res) = TimeTaker.measure { () =>
      i.evalExpr(expr, ls, inState)(anyValueCreator = i.defaultAnyValueCreator)
    }
    println(s"duration = ${dur.formatH}")
//    for (i <- res.info) {
//      println(s"info ${i.doc}")
//    }
    assert(res.value == true)
  }


}
