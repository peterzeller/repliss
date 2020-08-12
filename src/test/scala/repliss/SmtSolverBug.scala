package repliss

import crdtver.symbolic.smt.Cvc4Solver
import crdtver.symbolic.smt.Smt._

object SmtSolverBug {

  def main(args: Array[String]): Unit = {
    val t_InvocationId = Sort("InvocationId")
    val t_CallId = Sort("CallId")
    val currentInvocation = Variable("currentInvocation", t_InvocationId)
    val t_UserId = Sort("UserId")
    val id1 = Variable("id1", t_UserId)
    val t_String = Sort("String")
    val name = Variable("name", t_String)
    val mail = Variable("mail", t_String)
    val id2 = Variable("id2", t_UserId)
    val registerUser = DatatypeConstructor("registerUser", List(name, mail))
    val removeUser = DatatypeConstructor("removeUser", List(id1))
    val getUser = DatatypeConstructor("getUser", List(id2))
    val t_invocationInfo = Datatype(
      "invocationInfo",
      List(
        removeUser,
        registerUser,
        getUser))
    val invocationOp = Variable("invocationOp", ArrayType(t_InvocationId, t_invocationInfo))
    val name1 = Variable("name1", t_String)
    val mail1 = Variable("mail1", t_String)
    val notFound = DatatypeConstructor("notFound", List())
    val t_getUserResult = Datatype(
      "getUserResult",
      List(notFound, DatatypeConstructor("found", List(name1, mail1))))
    val getUser_res_arg = Variable("getUser_res_arg", t_getUserResult)
    val user_res = DatatypeConstructor("getUser_res", List(getUser_res_arg))
    val noResult = DatatypeConstructor("NoResult", List())
    val t_invocationResult = Datatype(
      "invocationResult",
      List(
        noResult,
        user_res))
    val invocationRes = Variable("invocationRes", ArrayType(t_InvocationId, t_invocationResult))
    val t_TxId = Sort("TxId")
    val Some_TxId_value = Variable("Some_TxId_value", t_TxId)
    val some_TxId = DatatypeConstructor("Some_TxId", List(Some_TxId_value))
    val none_TxId = DatatypeConstructor("None_TxId", List())
    val t_Option_TxId = Datatype(
      "Option_TxId",
      List(none_TxId, some_TxId))
    val Some_InvocationId_value = Variable("Some_InvocationId_value", t_InvocationId)
    val some_InvocationId = DatatypeConstructor("Some_InvocationId", List(Some_InvocationId_value))
    val none_InvocationId = DatatypeConstructor("None_InvocationId", List())
    val t_Option_InvocationId = Datatype(
      "Option_InvocationId",
      List(
        none_InvocationId,
        some_InvocationId))
    val key2 = Variable("key2", t_UserId)
    val key1 = Variable("key1", t_UserId)
    val value1 = Variable("value1", t_String)
    val key = Variable("key", t_UserId)
    val value = Variable("value", t_String)
    val user_name_assign = DatatypeConstructor("user_name_assign", List(key, value))
    val no_call = DatatypeConstructor("NoCall", List())
    val user_delete = DatatypeConstructor("user_delete", List(key2))
    val user_mail_assign = DatatypeConstructor("user_mail_assign", List(key1, value1))
    val t_callInfo = Datatype(
      "callInfo",
      List(
        user_delete,
        user_mail_assign,
        user_name_assign,
        no_call))
    val committed = DatatypeConstructor("Committed", List())
    val uncommitted = DatatypeConstructor("Uncommitted", List())
    val t_transactionStatus = Datatype(
      "transactionStatus",
      List(uncommitted, committed))
    val Some_transactionStatus_value = Variable("Some_transactionStatus_value", t_transactionStatus)
    val none_transactionStatus = DatatypeConstructor("None_transactionStatus", List())
    val some_transactionStatus = DatatypeConstructor("Some_transactionStatus", List(Some_transactionStatus_value))
    val t_Option_transactionStatus = Datatype(
      "Option_transactionStatus",
      List(
        none_transactionStatus,
        some_transactionStatus))
    val callOrigin1 = Variable("callOrigin1", ArrayType(t_CallId, t_Option_TxId))
    val transactionOrigin1 = Variable("transactionOrigin1", ArrayType(t_TxId, t_Option_InvocationId))
    val name_init = Variable("name_init", t_String)
    val mail_init = Variable("mail_init", t_String)
    val transactionStatus1 = Variable("transactionStatus1", ArrayType(t_TxId, t_Option_transactionStatus))
    val bound_tx4 = Variable("bound_tx4", t_TxId)
    val bound_c4 = Variable("bound_c4", t_CallId)
    val calls1 = Variable("calls1", ArrayType(t_CallId, t_callInfo))
    val bound_u5 = Variable("bound_u5", t_UserId)
    val bound_i7 = Variable("bound_i7", t_InvocationId)
    val bound_c6 = Variable("bound_c6", t_CallId)
    val tx = Variable("tx", t_TxId)
    val c0 = Variable("c0", t_CallId)
    val c1 = Variable("c1", t_CallId)
    val i1 = Variable("i1", t_InvocationId)
    val u4 = Variable("u4", t_UserId)
    val bound_c11 = Variable("bound_c11", t_CallId)
    val a14_transaction_begin_WF_transactionStatus_callOrigin = QuantifierExpr(
      Forall(),
      bound_tx4,
      Implies(
        Equals(
          MapSelect(transactionStatus1, bound_tx4),
          ApplyConstructor(
            t_Option_transactionStatus,
            none_transactionStatus,
            List())),
        QuantifierExpr(
          Forall(),
          bound_c4,
          Not(
            Equals(
              MapSelect(callOrigin1, bound_c4),
              ApplyConstructor(
                t_Option_TxId,
                some_TxId,
                List(bound_tx4)))))))
    val invocationOp2 = MapStore(
      invocationOp,
      currentInvocation,
      ApplyConstructor(
        t_invocationInfo,
        registerUser,
        List(name_init, mail_init)))
    val a18b = QuantifierExpr(
      Forall(),
      bound_u5,
      QuantifierExpr(
        Forall(),
        bound_i7,
        Implies(
          And(
            Equals(
              MapSelect(
                invocationOp2,
                bound_i7),
              ApplyConstructor(t_invocationInfo, removeUser, List(bound_u5))),
            Not(
              Equals(
                MapSelect(invocationRes, bound_i7),
                ApplyConstructor(t_invocationResult, noResult, List())))),
          QuantifierExpr(
            Exists(),
            bound_c6,
            And(
              Equals(
                IfThenElse(
                  ApplyTester(
                    t_Option_TxId,
                    none_TxId,
                    MapSelect(callOrigin1, bound_c6)),
                  ApplyConstructor(
                    t_Option_InvocationId,
                    none_InvocationId,
                    List()),
                  MapSelect(
                    transactionOrigin1,
                    ApplySelector(
                      t_Option_TxId,
                      some_TxId,
                      Some_TxId_value,
                      MapSelect(callOrigin1, bound_c6)))),
                ApplyConstructor(
                  t_Option_InvocationId,
                  some_InvocationId,
                  List(bound_i7))),
              Equals(
                MapSelect(calls1, bound_c6),
                ApplyConstructor(t_callInfo, user_delete, List(bound_u5))))))))
    val a18_invariant_before_transaction = a18b
    val a19_tx_fresh = Equals(
      MapSelect(transactionStatus1, tx),
      ApplyConstructor(t_Option_transactionStatus, none_transactionStatus, List()))
    val a26_c0_freshB = Equals(
      MapSelect(calls1, c0),
      ApplyConstructor(t_callInfo, no_call, List()))
    val calls2 = calls1
    val a28_c1_freshB = Equals(
      MapSelect(
        calls2,
        c1),
      ApplyConstructor(t_callInfo, no_call, List()))
    val transactionOrigin2 = MapStore(
      transactionOrigin1,
      tx,
      ApplyConstructor(
        t_Option_InvocationId,
        some_InvocationId,
        List(currentInvocation)))
    val callOrigin2 = callOrigin1
    val callOrigin3 = callOrigin2
    val calls3 =calls2
    //      MapStore(
//      calls2,
//      c1,
//      ApplyConstructor(
//        t_callInfo,
//        user_mail_assign,
//        List(u2, mail_init)))
    val a30_invariant_not_violated = Not(
      Implies(
        And(
          Equals(
            MapSelect(
              invocationOp2,
              i1),
            ApplyConstructor(t_invocationInfo, removeUser, List(u4))),
          Not(
            Equals(
              MapSelect(invocationRes, i1),
              ApplyConstructor(t_invocationResult, noResult, List())))),
        QuantifierExpr(
          Exists(),
          bound_c11,
          And(
            Equals(
              IfThenElse(
                ApplyTester(
                  t_Option_TxId,
                  none_TxId,
                  MapSelect(
                    callOrigin3,
                    bound_c11)),
                ApplyConstructor(t_Option_InvocationId, none_InvocationId, List()),
                MapSelect(
                  transactionOrigin2,
                  ApplySelector(
                    t_Option_TxId,
                    some_TxId,
                    Some_TxId_value,
                    MapSelect(
                      callOrigin3,
                      bound_c11)))),
              ApplyConstructor(
                t_Option_InvocationId,
                some_InvocationId,
                List(i1))),
            Equals(
              MapSelect(
                calls3,
                bound_c11),
              ApplyConstructor(t_callInfo, user_delete, List(u4)))))))

    val assertions = List(
      NamedConstraint("a14_transaction_begin_WF_transactionStatus_callOrigin", 3, a14_transaction_begin_WF_transactionStatus_callOrigin),
      NamedConstraint("a18_invariant_before_transaction", 3, a18_invariant_before_transaction),
      NamedConstraint("a19_tx_fresh", 3, a19_tx_fresh),
      NamedConstraint("a26_c0_freshB", 3, a26_c0_freshB),
      NamedConstraint("a28_c1_freshB", 3, a28_c1_freshB),
      NamedConstraint("a30_invariant_not_violated", 3, a30_invariant_not_violated))

    val solver = new Cvc4Solver()
    val res = solver.check(assertions, name = "bug")
    println(s"res = $res")

    //    println(s"min: ${findMin(assertions).get.map(_.description)}")
  }

  def findMin(cs: List[NamedConstraint]): Option[List[NamedConstraint]] = {
    try {
      println(s"checking ${cs.size}")
      val solver = new Cvc4Solver()
      solver.check(cs, name = "bug")
      None
    } catch {
      case _: Exception =>
        removedOne(cs).flatMap(findMin).headOption.orElse(Some(cs))
    }
  }



  /** stream of all the lists with one element removed */
  def removedOne[T](l: List[T]): LazyList[List[T]] = l match {
    case List() => LazyList()
    case x :: xs =>
      xs #:: removedOne(xs).map(ys => x :: ys)
  }

}
