package repliss

import crdtver.Repliss.{ReplissResult, SmallCheck, SmallCheck2}
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.FunctionKind.FunctionKindDatatypeConstructor
import crdtver.language.TypedAst._
import crdtver.testing.SmallcheckTester2
import crdtver.testing.SmallcheckTester2.StateEq
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.{Helper, TimeTaker}
import crdtver.{Repliss, RunArgs}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.concurrent.duration.DurationInt

/**
 * Tests for the random test generator
 */
class Smallcheck2Tests extends AnyFunSuite with Matchers {

  //  def checkResource(name: String): Result[ReplissResult] = {
  //    val input = Helper.getResource(name)
  //    Repliss.checkInput(input, name, runArgs = RunArgs())
  //  }

  private def checkString(name: String, input: String, runArgs: RunArgs): ReplissResult = {
    val res = Repliss.checkInput(input, name, runArgs = runArgs, checks = List(SmallCheck2()))
    res match {
      case Repliss.NormalResult(rr) =>
        Repliss.printTestingResultSmallCheck(rr, name, new Object())
        rr
      case Repliss.ErrorResult(errors) =>
        throw new RuntimeException(errors.map(_.toString).mkString("\n"))
    }
  }

  private def checkResource(name: String, runArgs: RunArgs): ReplissResult = {
    val input = Helper.getResource(name)
    checkString(name, input, runArgs)
  }


  test("userbase_fail1 counterexample", Slow) {
    val res = checkResource("/examples/buggy/userbase_fail1.rpls", RunArgs(timeout = 10.minutes))

    assert(res.hasSmallCheckCounterexample)
  }

  test("no counterexample for userbase", Slow) {

    val res = checkResource("/examples/verified/userbase.rpls", RunArgs(timeout = 1.minutes))

    assert(!res.hasSmallCheckCounterexample)
  }


  test("state equality") {
    import crdtver.testing.Interpreter
    import Interpreter._

    val state = State(
      Map(
        CallId(12) -> CallInfo(
          CallId(12),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 1)))))))))),
          SnapshotTime(Set(CallId(10), CallId(11), CallId(12))),
          TransactionId(2),
          InvocationId(2)),
        CallId(9) -> CallInfo(
          CallId(9),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "DeleteKey_MessageId_StructAuthorContentOp",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(9), CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
          TransactionId(4),
          InvocationId(4)),
        CallId(4) -> CallInfo(
          CallId(4),
          DataTypeValue(
            "Qry",
            List(
              AnyValue(
                DataTypeValue(
                  "messageQry",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "ContainsKey_MessageId_StructAuthorContentQuery",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(1), CallId(2), CallId(3), CallId(4))),
          TransactionId(3),
          InvocationId(3)),
        CallId(3) -> CallInfo(
          CallId(3),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(1), CallId(2), CallId(3))),
          TransactionId(1),
          InvocationId(1)),
        CallId(8) -> CallInfo(
          CallId(8),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
          TransactionId(4),
          InvocationId(4)),
        CallId(5) -> CallInfo(
          CallId(5),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(1), CallId(2))),
          TransactionId(3),
          InvocationId(3)),
        CallId(10) -> CallInfo(
          CallId(10),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 1)),
                          AnyValue(
                            DataTypeValue(
                              "author",
                              List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
          SnapshotTime(Set(CallId(10))),
          TransactionId(2),
          InvocationId(2)),
        CallId(7) -> CallInfo(
          CallId(7),
          DataTypeValue(
            "Qry",
            List(
              AnyValue(
                DataTypeValue(
                  "messageQry",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "ContainsKey_MessageId_StructAuthorContentQuery",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(3), CallId(1), CallId(2), CallId(7))),
          TransactionId(4),
          InvocationId(4)),
        CallId(1) -> CallInfo(
          CallId(1),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 0)),
                          AnyValue(
                            DataTypeValue(
                              "author",
                              List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
          SnapshotTime(Set(CallId(1))),
          TransactionId(1),
          InvocationId(1)),
        CallId(2) -> CallInfo(
          CallId(2),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 0)),
                          AnyValue(
                            DataTypeValue(
                              "content",
                              List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
          SnapshotTime(Set(CallId(1), CallId(2))),
          TransactionId(1),
          InvocationId(1)),
        CallId(6) -> CallInfo(
          CallId(6),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "DeleteKey_MessageId_StructAuthorContentOp",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(6), CallId(1), CallId(2))),
          TransactionId(3),
          InvocationId(3)),
        CallId(11) -> CallInfo(
          CallId(11),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 1)),
                          AnyValue(
                            DataTypeValue(
                              "content",
                              List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
          SnapshotTime(Set(CallId(10), CallId(11))),
          TransactionId(2),
          InvocationId(2))),
      12,
      Map(
        TransactionId(1) -> TransactionInfo(
          TransactionId(1),
          SnapshotTime(Set()),
          InvocationId(1),
          List(
            CallInfo(
              CallId(1),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 0)),
                              AnyValue(
                                DataTypeValue(
                                  "author",
                                  List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
              SnapshotTime(Set(CallId(1))),
              TransactionId(1),
              InvocationId(1)),
            CallInfo(
              CallId(2),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 0)),
                              AnyValue(
                                DataTypeValue(
                                  "content",
                                  List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
              SnapshotTime(Set(CallId(1), CallId(2))),
              TransactionId(1),
              InvocationId(1)),
            CallInfo(
              CallId(3),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(1), CallId(2), CallId(3))),
              TransactionId(1),
              InvocationId(1))),
          true),
        TransactionId(3) -> TransactionInfo(
          TransactionId(3),
          SnapshotTime(Set(CallId(1), CallId(2), CallId(3))),
          InvocationId(3),
          List(
            CallInfo(
              CallId(4),
              DataTypeValue(
                "Qry",
                List(
                  AnyValue(
                    DataTypeValue(
                      "messageQry",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "ContainsKey_MessageId_StructAuthorContentQuery",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(1), CallId(2), CallId(3), CallId(4))),
              TransactionId(3),
              InvocationId(3)),
            CallInfo(
              CallId(5),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(1), CallId(2))),
              TransactionId(3),
              InvocationId(3)),
            CallInfo(
              CallId(6),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "DeleteKey_MessageId_StructAuthorContentOp",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(6), CallId(1), CallId(2))),
              TransactionId(3),
              InvocationId(3))),
          true),
        TransactionId(4) -> TransactionInfo(
          TransactionId(4),
          SnapshotTime(Set(CallId(3), CallId(1), CallId(2))),
          InvocationId(4),
          List(
            CallInfo(
              CallId(7),
              DataTypeValue(
                "Qry",
                List(
                  AnyValue(
                    DataTypeValue(
                      "messageQry",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "ContainsKey_MessageId_StructAuthorContentQuery",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(3), CallId(1), CallId(2), CallId(7))),
              TransactionId(4),
              InvocationId(4)),
            CallInfo(
              CallId(8),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
              TransactionId(4),
              InvocationId(4)),
            CallInfo(
              CallId(9),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "DeleteKey_MessageId_StructAuthorContentOp",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(9), CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
              TransactionId(4),
              InvocationId(4))),
          true),
        TransactionId(2) -> TransactionInfo(
          TransactionId(2),
          SnapshotTime(Set()),
          InvocationId(2),
          List(
            CallInfo(
              CallId(10),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 1)),
                              AnyValue(
                                DataTypeValue(
                                  "author",
                                  List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
              SnapshotTime(Set(CallId(10))),
              TransactionId(2),
              InvocationId(2)),
            CallInfo(
              CallId(11),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 1)),
                              AnyValue(
                                DataTypeValue(
                                  "content",
                                  List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
              SnapshotTime(Set(CallId(10), CallId(11))),
              TransactionId(2),
              InvocationId(2)),
            CallInfo(
              CallId(12),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 1)))))))))),
              SnapshotTime(Set(CallId(10), CallId(11), CallId(12))),
              TransactionId(2),
              InvocationId(2))),
          true)),
      5,
      Map(
        InvocationId(4) -> InvocationInfo(
          InvocationId(4),
          DataTypeValue("deleteMessage", List(AnyValue(DomainValue("MessageId", 0)))),
          Some(DataTypeValue("deleteMessage_res", List(AnyValue("nothing"))))),
        InvocationId(5) -> InvocationInfo(
          InvocationId(5),
          DataTypeValue("sendMessage", List(AnyValue(DomainValue("UserId", 0)), AnyValue(DomainValue("String", 0)))),
          None),
        InvocationId(3) -> InvocationInfo(
          InvocationId(3),
          DataTypeValue("deleteMessage", List(AnyValue(DomainValue("MessageId", 0)))),
          Some(DataTypeValue("deleteMessage_res", List(AnyValue("nothing"))))),
        InvocationId(1) -> InvocationInfo(
          InvocationId(1),
          DataTypeValue("sendMessage", List(AnyValue(DomainValue("UserId", 0)), AnyValue(DomainValue("String", 0)))),
          Some(DataTypeValue("sendMessage_res", List(AnyValue(DomainValue("MessageId", 0)))))),
        InvocationId(2) -> InvocationInfo(
          InvocationId(2),
          DataTypeValue("sendMessage", List(AnyValue(DomainValue("UserId", 0)), AnyValue(DomainValue("String", 0)))),
          Some(DataTypeValue("sendMessage_res", List(AnyValue(DomainValue("MessageId", 1))))))),
      5,
      Map(
        IdType("MessageId")() -> Map(
          AnyValue(DomainValue("MessageId", 0)) -> InvocationId(1),
          AnyValue(DomainValue("MessageId", 1)) -> InvocationId(2))),
      Map(IdType("MessageId")() -> Set(AnyValue(DomainValue("MessageId", 0)), AnyValue(DomainValue("MessageId", 1)))),
      Map(
        InvocationId(5) -> LocalState(
          Some(InvocationId(5)),
          Map(
            LocalVar("from") -> AnyValue(DomainValue("UserId", 0)),
            LocalVar("text") -> AnyValue(DomainValue("String", 0))),
          List(
            ExecStmt(
              CrdtCall(
                NoSource(),
                FunctionCall(
                  NoSource(),
                  CallInfoType(),
                  Identifier(NoSource(), "Op"),
                  List(),
                  List(
                    FunctionCall(
                      NoSource(),
                      SimpleType("rootCrdtOp", List())(),
                      Identifier(NoSource(), "message"),
                      List(),
                      List(
                        FunctionCall(
                          NoSource(),
                          SimpleType("MapOp_MessageId_StructAuthorContentOp", List())(),
                          Identifier(NoSource(), "NestedOp_MessageId_StructAuthorContentOp"),
                          List(),
                          List(
                            VarUse(NoSource(), IdType("MessageId")(), "m"),
                            FunctionCall(
                              NoSource(),
                              SimpleType("StructAuthorContentOp", List())(),
                              Identifier(NoSource(), "author"),
                              List(),
                              List(
                                FunctionCall(
                                  NoSource(),
                                  SimpleType("RegisterOp_UserId", List())(),
                                  Identifier(NoSource(), "Assign_UserId"),
                                  List(),
                                  List(VarUse(NoSource(), SimpleType("UserId", List())(), "from")),
                                  FunctionKindDatatypeConstructor())),
                              FunctionKindDatatypeConstructor())),
                          FunctionKindDatatypeConstructor())),
                      FunctionKindDatatypeConstructor())),
                  FunctionKindDatatypeConstructor()))),
            ExecStmt(
              CrdtCall(
                NoSource(),
                FunctionCall(
                  NoSource(),
                  CallInfoType(),
                  Identifier(NoSource(), "Op"),
                  List(),
                  List(
                    FunctionCall(
                      NoSource(),
                      SimpleType("rootCrdtOp", List())(),
                      Identifier(NoSource(), "message"),
                      List(),
                      List(
                        FunctionCall(
                          NoSource(),
                          SimpleType("MapOp_MessageId_StructAuthorContentOp", List())(),
                          Identifier(NoSource(), "NestedOp_MessageId_StructAuthorContentOp"),
                          List(),
                          List(
                            VarUse(NoSource(), IdType("MessageId")(), "m"),
                            FunctionCall(
                              NoSource(),
                              SimpleType("StructAuthorContentOp", List())(),
                              Identifier(NoSource(), "content"),
                              List(),
                              List(
                                FunctionCall(
                                  NoSource(),
                                  SimpleType("RegisterOp_String", List())(),
                                  Identifier(NoSource(), "Assign_String"),
                                  List(),
                                  List(VarUse(NoSource(), SimpleType("String", List())(), "text")),
                                  FunctionKindDatatypeConstructor())),
                              FunctionKindDatatypeConstructor())),
                          FunctionKindDatatypeConstructor())),
                      FunctionKindDatatypeConstructor())),
                  FunctionKindDatatypeConstructor()))),
            ExecStmt(
              CrdtCall(
                NoSource(),
                FunctionCall(
                  NoSource(),
                  CallInfoType(),
                  Identifier(NoSource(), "Op"),
                  List(),
                  List(
                    FunctionCall(
                      NoSource(),
                      SimpleType("rootCrdtOp", List())(),
                      Identifier(NoSource(), "chat"),
                      List(),
                      List(
                        FunctionCall(
                          NoSource(),
                          SimpleType("SetOp_MessageId", List())(),
                          Identifier(NoSource(), "Add_MessageId"),
                          List(),
                          List(VarUse(NoSource(), IdType("MessageId")(), "m")),
                          FunctionKindDatatypeConstructor())),
                      FunctionKindDatatypeConstructor())),
                  FunctionKindDatatypeConstructor()))),
            EndAtomic(),
            ExecStmt(ReturnStmt(NoSource(), VarUse(NoSource(), IdType("MessageId")(), "m"), List()))),
          WaitForNewId("m", IdType("MessageId")()),
          Some(
            TransactionInfo(
              TransactionId(5),
              SnapshotTime(
                Set(CallId(12), CallId(4), CallId(3), CallId(5), CallId(10), CallId(1), CallId(2), CallId(6), CallId(11))),
              InvocationId(5),
              List(),
              false)),
          Set(CallId(12), CallId(4), CallId(3), CallId(5), CallId(10), CallId(1), CallId(2), CallId(6), CallId(11)))))
    val oldState = State(
      Map(
        CallId(12) -> CallInfo(
          CallId(12),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 1)))))))))),
          SnapshotTime(Set(CallId(10), CallId(11), CallId(12))),
          TransactionId(2),
          InvocationId(2)),
        CallId(9) -> CallInfo(
          CallId(9),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "DeleteKey_MessageId_StructAuthorContentOp",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(9), CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
          TransactionId(4),
          InvocationId(4)),
        CallId(4) -> CallInfo(
          CallId(4),
          DataTypeValue(
            "Qry",
            List(
              AnyValue(
                DataTypeValue(
                  "messageQry",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "ContainsKey_MessageId_StructAuthorContentQuery",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(1), CallId(2), CallId(3), CallId(4))),
          TransactionId(3),
          InvocationId(3)),
        CallId(3) -> CallInfo(
          CallId(3),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(1), CallId(2), CallId(3))),
          TransactionId(1),
          InvocationId(1)),
        CallId(8) -> CallInfo(
          CallId(8),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
          TransactionId(4),
          InvocationId(4)),
        CallId(5) -> CallInfo(
          CallId(5),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "chat",
                  List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(1), CallId(2))),
          TransactionId(3),
          InvocationId(3)),
        CallId(10) -> CallInfo(
          CallId(10),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 1)),
                          AnyValue(
                            DataTypeValue(
                              "author",
                              List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
          SnapshotTime(Set(CallId(10))),
          TransactionId(2),
          InvocationId(2)),
        CallId(7) -> CallInfo(
          CallId(7),
          DataTypeValue(
            "Qry",
            List(
              AnyValue(
                DataTypeValue(
                  "messageQry",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "ContainsKey_MessageId_StructAuthorContentQuery",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(3), CallId(1), CallId(2), CallId(7))),
          TransactionId(4),
          InvocationId(4)),
        CallId(1) -> CallInfo(
          CallId(1),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 0)),
                          AnyValue(
                            DataTypeValue(
                              "author",
                              List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
          SnapshotTime(Set(CallId(1))),
          TransactionId(1),
          InvocationId(1)),
        CallId(2) -> CallInfo(
          CallId(2),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 0)),
                          AnyValue(
                            DataTypeValue(
                              "content",
                              List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
          SnapshotTime(Set(CallId(1), CallId(2))),
          TransactionId(1),
          InvocationId(1)),
        CallId(6) -> CallInfo(
          CallId(6),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "DeleteKey_MessageId_StructAuthorContentOp",
                        List(AnyValue(DomainValue("MessageId", 0)))))))))),
          SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(6), CallId(1), CallId(2))),
          TransactionId(3),
          InvocationId(3)),
        CallId(11) -> CallInfo(
          CallId(11),
          DataTypeValue(
            "Op",
            List(
              AnyValue(
                DataTypeValue(
                  "message",
                  List(
                    AnyValue(
                      DataTypeValue(
                        "NestedOp_MessageId_StructAuthorContentOp",
                        List(
                          AnyValue(DomainValue("MessageId", 1)),
                          AnyValue(
                            DataTypeValue(
                              "content",
                              List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
          SnapshotTime(Set(CallId(10), CallId(11))),
          TransactionId(2),
          InvocationId(2))),
      12,
      Map(
        TransactionId(1) -> TransactionInfo(
          TransactionId(1),
          SnapshotTime(Set()),
          InvocationId(1),
          List(
            CallInfo(
              CallId(1),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 0)),
                              AnyValue(
                                DataTypeValue(
                                  "author",
                                  List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
              SnapshotTime(Set(CallId(1))),
              TransactionId(1),
              InvocationId(1)),
            CallInfo(
              CallId(2),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 0)),
                              AnyValue(
                                DataTypeValue(
                                  "content",
                                  List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
              SnapshotTime(Set(CallId(1), CallId(2))),
              TransactionId(1),
              InvocationId(1)),
            CallInfo(
              CallId(3),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(1), CallId(2), CallId(3))),
              TransactionId(1),
              InvocationId(1))),
          true),
        TransactionId(3) -> TransactionInfo(
          TransactionId(3),
          SnapshotTime(Set(CallId(1), CallId(2), CallId(3))),
          InvocationId(3),
          List(
            CallInfo(
              CallId(4),
              DataTypeValue(
                "Qry",
                List(
                  AnyValue(
                    DataTypeValue(
                      "messageQry",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "ContainsKey_MessageId_StructAuthorContentQuery",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(1), CallId(2), CallId(3), CallId(4))),
              TransactionId(3),
              InvocationId(3)),
            CallInfo(
              CallId(5),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(1), CallId(2))),
              TransactionId(3),
              InvocationId(3)),
            CallInfo(
              CallId(6),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "DeleteKey_MessageId_StructAuthorContentOp",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(4), CallId(3), CallId(5), CallId(6), CallId(1), CallId(2))),
              TransactionId(3),
              InvocationId(3))),
          true),
        TransactionId(4) -> TransactionInfo(
          TransactionId(4),
          SnapshotTime(Set(CallId(3), CallId(1), CallId(2))),
          InvocationId(4),
          List(
            CallInfo(
              CallId(7),
              DataTypeValue(
                "Qry",
                List(
                  AnyValue(
                    DataTypeValue(
                      "messageQry",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "ContainsKey_MessageId_StructAuthorContentQuery",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(3), CallId(1), CallId(2), CallId(7))),
              TransactionId(4),
              InvocationId(4)),
            CallInfo(
              CallId(8),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Remove_MessageId", List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
              TransactionId(4),
              InvocationId(4)),
            CallInfo(
              CallId(9),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "DeleteKey_MessageId_StructAuthorContentOp",
                            List(AnyValue(DomainValue("MessageId", 0)))))))))),
              SnapshotTime(Set(CallId(9), CallId(3), CallId(8), CallId(7), CallId(1), CallId(2))),
              TransactionId(4),
              InvocationId(4))),
          true),
        TransactionId(2) -> TransactionInfo(
          TransactionId(2),
          SnapshotTime(Set()),
          InvocationId(2),
          List(
            CallInfo(
              CallId(10),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 1)),
                              AnyValue(
                                DataTypeValue(
                                  "author",
                                  List(AnyValue(DataTypeValue("Assign_UserId", List(AnyValue(DomainValue("UserId", 0)))))))))))))))),
              SnapshotTime(Set(CallId(10))),
              TransactionId(2),
              InvocationId(2)),
            CallInfo(
              CallId(11),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "message",
                      List(
                        AnyValue(
                          DataTypeValue(
                            "NestedOp_MessageId_StructAuthorContentOp",
                            List(
                              AnyValue(DomainValue("MessageId", 1)),
                              AnyValue(
                                DataTypeValue(
                                  "content",
                                  List(AnyValue(DataTypeValue("Assign_String", List(AnyValue(DomainValue("String", 0)))))))))))))))),
              SnapshotTime(Set(CallId(10), CallId(11))),
              TransactionId(2),
              InvocationId(2)),
            CallInfo(
              CallId(12),
              DataTypeValue(
                "Op",
                List(
                  AnyValue(
                    DataTypeValue(
                      "chat",
                      List(AnyValue(DataTypeValue("Add_MessageId", List(AnyValue(DomainValue("MessageId", 1)))))))))),
              SnapshotTime(Set(CallId(10), CallId(11), CallId(12))),
              TransactionId(2),
              InvocationId(2))),
          true)),
      5,
      Map(
        InvocationId(4) -> InvocationInfo(
          InvocationId(4),
          DataTypeValue("deleteMessage", List(AnyValue(DomainValue("MessageId", 0)))),
          Some(DataTypeValue("deleteMessage_res", List(AnyValue("nothing"))))),
        InvocationId(5) -> InvocationInfo(
          InvocationId(5),
          DataTypeValue("sendMessage", List(AnyValue(DomainValue("UserId", 0)), AnyValue(DomainValue("String", 0)))),
          None),
        InvocationId(3) -> InvocationInfo(
          InvocationId(3),
          DataTypeValue("deleteMessage", List(AnyValue(DomainValue("MessageId", 0)))),
          Some(DataTypeValue("deleteMessage_res", List(AnyValue("nothing"))))),
        InvocationId(1) -> InvocationInfo(
          InvocationId(1),
          DataTypeValue("sendMessage", List(AnyValue(DomainValue("UserId", 0)), AnyValue(DomainValue("String", 0)))),
          Some(DataTypeValue("sendMessage_res", List(AnyValue(DomainValue("MessageId", 0)))))),
        InvocationId(2) -> InvocationInfo(
          InvocationId(2),
          DataTypeValue("sendMessage", List(AnyValue(DomainValue("UserId", 0)), AnyValue(DomainValue("String", 0)))),
          Some(DataTypeValue("sendMessage_res", List(AnyValue(DomainValue("MessageId", 1))))))),
      5,
      Map(
        IdType("MessageId")() -> Map(
          AnyValue(DomainValue("MessageId", 0)) -> InvocationId(1),
          AnyValue(DomainValue("MessageId", 1)) -> InvocationId(2))),
      Map(IdType("MessageId")() -> Set(AnyValue(DomainValue("MessageId", 0)), AnyValue(DomainValue("MessageId", 1)))),
      Map(
        InvocationId(5) -> LocalState(
          Some(InvocationId(5)),
          Map(
            LocalVar("from") -> AnyValue(DomainValue("UserId", 0)),
            LocalVar("text") -> AnyValue(DomainValue("String", 0))),
          List(
            ExecStmt(
              CrdtCall(
                NoSource(),
                FunctionCall(
                  NoSource(),
                  CallInfoType(),
                  Identifier(NoSource(), "Op"),
                  List(),
                  List(
                    FunctionCall(
                      NoSource(),
                      SimpleType("rootCrdtOp", List())(),
                      Identifier(NoSource(), "message"),
                      List(),
                      List(
                        FunctionCall(
                          NoSource(),
                          SimpleType("MapOp_MessageId_StructAuthorContentOp", List())(),
                          Identifier(NoSource(), "NestedOp_MessageId_StructAuthorContentOp"),
                          List(),
                          List(
                            VarUse(NoSource(), IdType("MessageId")(), "m"),
                            FunctionCall(
                              NoSource(),
                              SimpleType("StructAuthorContentOp", List())(),
                              Identifier(NoSource(), "author"),
                              List(),
                              List(
                                FunctionCall(
                                  NoSource(),
                                  SimpleType("RegisterOp_UserId", List())(),
                                  Identifier(NoSource(), "Assign_UserId"),
                                  List(),
                                  List(VarUse(NoSource(), SimpleType("UserId", List())(), "from")),
                                  FunctionKindDatatypeConstructor())),
                              FunctionKindDatatypeConstructor())),
                          FunctionKindDatatypeConstructor())),
                      FunctionKindDatatypeConstructor())),
                  FunctionKindDatatypeConstructor()))),
            ExecStmt(
              CrdtCall(
                NoSource(),
                FunctionCall(
                  NoSource(),
                  CallInfoType(),
                  Identifier(NoSource(), "Op"),
                  List(),
                  List(
                    FunctionCall(
                      NoSource(),
                      SimpleType("rootCrdtOp", List())(),
                      Identifier(NoSource(), "message"),
                      List(),
                      List(
                        FunctionCall(
                          NoSource(),
                          SimpleType("MapOp_MessageId_StructAuthorContentOp", List())(),
                          Identifier(NoSource(), "NestedOp_MessageId_StructAuthorContentOp"),
                          List(),
                          List(
                            VarUse(NoSource(), IdType("MessageId")(), "m"),
                            FunctionCall(
                              NoSource(),
                              SimpleType("StructAuthorContentOp", List())(),
                              Identifier(NoSource(), "content"),
                              List(),
                              List(
                                FunctionCall(
                                  NoSource(),
                                  SimpleType("RegisterOp_String", List())(),
                                  Identifier(NoSource(), "Assign_String"),
                                  List(),
                                  List(VarUse(NoSource(), SimpleType("String", List())(), "text")),
                                  FunctionKindDatatypeConstructor())),
                              FunctionKindDatatypeConstructor())),
                          FunctionKindDatatypeConstructor())),
                      FunctionKindDatatypeConstructor())),
                  FunctionKindDatatypeConstructor()))),
            ExecStmt(
              CrdtCall(
                NoSource(),
                FunctionCall(
                  NoSource(),
                  CallInfoType(),
                  Identifier(NoSource(), "Op"),
                  List(),
                  List(
                    FunctionCall(
                      NoSource(),
                      SimpleType("rootCrdtOp", List())(),
                      Identifier(NoSource(), "chat"),
                      List(),
                      List(
                        FunctionCall(
                          NoSource(),
                          SimpleType("SetOp_MessageId", List())(),
                          Identifier(NoSource(), "Add_MessageId"),
                          List(),
                          List(VarUse(NoSource(), IdType("MessageId")(), "m")),
                          FunctionKindDatatypeConstructor())),
                      FunctionKindDatatypeConstructor())),
                  FunctionKindDatatypeConstructor()))),
            EndAtomic(),
            ExecStmt(ReturnStmt(NoSource(), VarUse(NoSource(), IdType("MessageId")(), "m"), List()))),
          WaitForNewId("m", IdType("MessageId")()),
          Some(
            TransactionInfo(
              TransactionId(5),
              SnapshotTime(
                Set(CallId(9), CallId(4), CallId(3), CallId(8), CallId(5), CallId(6), CallId(7), CallId(1), CallId(2))),
              InvocationId(5),
              List(),
              false)),
          Set(CallId(9), CallId(4), CallId(3), CallId(8), CallId(5), CallId(6), CallId(7), CallId(1), CallId(2)))))


    val (dur, eq) = TimeTaker.measure { () =>
      StateEq.statesEquivalent(state, oldState)
    }
    println(s"dur = ${dur.formatH}")
    println(s"eq = $eq")

  }


}
