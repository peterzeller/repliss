package test;

import edu.nyu.acsys.CVC4.*;
import org.jetbrains.annotations.NotNull;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static java.nio.charset.StandardCharsets.UTF_8;

@SuppressWarnings("Duplicates")
class Cvc4Test2 {

    private static int count = 0;
    private static Map<String, Expr> assertions = new LinkedHashMap<>();

    public static void main(String... args) throws IOException {
        System.loadLibrary("cvc4jni");
        ExprManager em = new ExprManager();
        SmtEngine smt = new SmtEngine(em);

        smt.setOption("produce-models", new SExpr(true));
        smt.setOption("finite-model-find", new SExpr(true));
        smt.setOption("e-matching", new SExpr(true));
        smt.setOption("incremental", new SExpr(true));
        smt.setOption("tlimit", new SExpr(30000));
        smt.setOption("produce-assertions", new SExpr(true));
        smt.setOption("output-language", new SExpr("cvc4"));
        smt.setOption("default-dag-thresh", new SExpr(0));
        smt.setOption("produce-unsat-cores", new SExpr(true));
        SortType invocationId = em.mkSort("InvocationId");
        DatatypeConstructor removeUser = new DatatypeConstructor("removeUser");
        SortType userId = em.mkSort("UserId");
        removeUser.addArg("id1", userId);
        DatatypeConstructor updateMail = new DatatypeConstructor("updateMail");
        updateMail.addArg("id", userId);
        SortType string = em.mkSort("String");
        updateMail.addArg("newMail", string);
        DatatypeConstructor registerUser = new DatatypeConstructor("registerUser");
        registerUser.addArg("name", string);
        registerUser.addArg("mail", string);
        DatatypeConstructor getUser = new DatatypeConstructor("getUser");
        getUser.addArg("id2", userId);
        DatatypeConstructor no_invocation = new DatatypeConstructor("no_invocation");
        Datatype invocationInfo = new Datatype("invocationInfo");
        invocationInfo.addConstructor(removeUser);
        invocationInfo.addConstructor(updateMail);
        invocationInfo.addConstructor(registerUser);
        invocationInfo.addConstructor(getUser);
        invocationInfo.addConstructor(no_invocation);
        DatatypeType invocationInfoDt = em.mkDatatypeType(invocationInfo);
        DatatypeConstructor removeUser1 = invocationInfoDt.getDatatype().get("removeUser");
        DatatypeConstructor registerUser1 = invocationInfoDt.getDatatype().get("registerUser");
        Expr invocationOp = em.mkVar("invocationOp", em.mkArrayType(invocationId, invocationInfoDt));
        Expr currentInvocation = em.mkVar("currentInvocation", invocationId);
        SortType txId = em.mkSort("TxId");
        DatatypeConstructor uncommitted = new DatatypeConstructor("Uncommitted");
        DatatypeConstructor committed = new DatatypeConstructor("Committed");
        Datatype transactionStatus = new Datatype("transactionStatus");
        transactionStatus.addConstructor(uncommitted);
        transactionStatus.addConstructor(committed);
        DatatypeType transactionStatusDt = em.mkDatatypeType(transactionStatus);
        Datatype option_transactionStatus = new Datatype("Option_transactionStatus");
        DatatypeConstructor none_transactionStatus = new DatatypeConstructor("None_transactionStatus");
        DatatypeConstructor someTransactionStatus = new DatatypeConstructor("Some_transactionStatus");
        someTransactionStatus.addArg("Some_transactionStatus_value", transactionStatusDt);
        option_transactionStatus.addConstructor(none_transactionStatus);
        option_transactionStatus.addConstructor(someTransactionStatus);
        SortType callId = em.mkSort("CallId");
        SetType callIdSet = em.mkSetType(callId);
        DatatypeConstructor noResult = new DatatypeConstructor("NoResult");
        DatatypeConstructor getUser_res = new DatatypeConstructor("getUser_res");
        DatatypeConstructor notFound = new DatatypeConstructor("notFound");
        DatatypeConstructor found = new DatatypeConstructor("found");
        found.addArg("name1", string);
        found.addArg("mail1", string);
        Datatype getUserResult = new Datatype("getUserResult");
        getUserResult.addConstructor(notFound);
        getUserResult.addConstructor(found);
        DatatypeType getUserResultDt = em.mkDatatypeType(getUserResult);
        getUser_res.addArg("getUser_res_arg", getUserResultDt);
        DatatypeConstructor registerUser_res = new DatatypeConstructor("registerUser_res");
        registerUser_res.addArg("registerUser_res_arg", userId);
        DatatypeConstructor removeUser_res = new DatatypeConstructor("removeUser_res");
        DatatypeConstructor updateMail_res = new DatatypeConstructor("updateMail_res");
        Datatype invocationResult = new Datatype("invocationResult");
        invocationResult.addConstructor(noResult);
        invocationResult.addConstructor(getUser_res);
        invocationResult.addConstructor(registerUser_res);
        invocationResult.addConstructor(removeUser_res);
        invocationResult.addConstructor(updateMail_res);
        DatatypeType invocationResultDt = em.mkDatatypeType(invocationResult);
        DatatypeConstructor noResult1 = invocationResultDt.getDatatype().get("NoResult");
        ArrayType arrayType = em.mkArrayType(invocationId, invocationResultDt);
        Expr invocationRes = em.mkVar("invocationRes", arrayType);
        Datatype option_txId = new Datatype("Option_TxId");
        DatatypeConstructor none_txId = new DatatypeConstructor("None_TxId");
        DatatypeConstructor someTxId = new DatatypeConstructor("Some_TxId");
        someTxId.addArg("Some_TxId_value", txId);
        option_txId.addConstructor(none_txId);
        option_txId.addConstructor(someTxId);
        DatatypeType option_txIdDt = em.mkDatatypeType(option_txId);
        DatatypeConstructor none_txId1 = option_txIdDt.getDatatype().get("None_TxId");
        DatatypeConstructor some_txId = option_txIdDt.getDatatype().get("Some_TxId");
        Datatype option_invocationId = new Datatype("Option_InvocationId");
        DatatypeConstructor none_invocationId = new DatatypeConstructor("None_InvocationId");
        DatatypeConstructor someInvocationId = new DatatypeConstructor("Some_InvocationId");
        someInvocationId.addArg("Some_InvocationId_value", invocationId);
        option_invocationId.addConstructor(none_invocationId);
        option_invocationId.addConstructor(someInvocationId);
        DatatypeType option_invocationIdDt = em.mkDatatypeType(option_invocationId);
        DatatypeConstructor none_invocationId1 = option_invocationIdDt.getDatatype().get("None_InvocationId");
        DatatypeConstructor someInvocationId1 = option_invocationIdDt.getDatatype().get("Some_InvocationId");
        DatatypeConstructor queryop_user_mail_get = new DatatypeConstructor("queryop_user_mail_get");
        queryop_user_mail_get.addArg("key5", userId);
        queryop_user_mail_get.addArg("result2", string);
        DatatypeConstructor queryop_user_name_isEqualTo = new DatatypeConstructor("queryop_user_name_isEqualTo");
        queryop_user_name_isEqualTo.addArg("key4", userId);
        queryop_user_name_isEqualTo.addArg("other", string);
        queryop_user_name_isEqualTo.addArg("result1", em.booleanType());
        DatatypeConstructor user_delete = new DatatypeConstructor("user_delete");
        user_delete.addArg("key2", userId);
        DatatypeConstructor queryop_user_mail_isEqualTo = new DatatypeConstructor("queryop_user_mail_isEqualTo");
        queryop_user_mail_isEqualTo.addArg("key6", userId);
        queryop_user_mail_isEqualTo.addArg("other1", string);
        queryop_user_mail_isEqualTo.addArg("result3", em.booleanType());
        DatatypeConstructor user_mail_assign = new DatatypeConstructor("user_mail_assign");
        user_mail_assign.addArg("key1", userId);
        user_mail_assign.addArg("value1", string);
        DatatypeConstructor user_name_assign = new DatatypeConstructor("user_name_assign");
        user_name_assign.addArg("key", userId);
        user_name_assign.addArg("value", string);
        DatatypeConstructor queryop_user_name_get = new DatatypeConstructor("queryop_user_name_get");
        queryop_user_name_get.addArg("key3", userId);
        queryop_user_name_get.addArg("result", string);
        DatatypeConstructor queryop_user_exists = new DatatypeConstructor("queryop_user_exists");
        queryop_user_exists.addArg("key7", userId);
        queryop_user_exists.addArg("result4", em.booleanType());
        DatatypeConstructor no_call = new DatatypeConstructor("no_call");
        Datatype callInfo = new Datatype("callInfo");
        callInfo.addConstructor(queryop_user_mail_get);
        callInfo.addConstructor(queryop_user_name_isEqualTo);
        callInfo.addConstructor(user_delete);
        callInfo.addConstructor(queryop_user_mail_isEqualTo);
        callInfo.addConstructor(user_mail_assign);
        callInfo.addConstructor(user_name_assign);
        callInfo.addConstructor(queryop_user_name_get);
        callInfo.addConstructor(queryop_user_exists);
        callInfo.addConstructor(no_call);
        DatatypeType callInfoDt = em.mkDatatypeType(callInfo);
        DatatypeConstructor user_delete1 = callInfoDt.getDatatype().get("user_delete");
        DatatypeConstructor user_mail_assign1 = callInfoDt.getDatatype().get("user_mail_assign");
        DatatypeConstructor user_name_assign1 = callInfoDt.getDatatype().get("user_name_assign");
        DatatypeConstructor no_call1 = callInfoDt.getDatatype().get("no_call");
        Expr name_init = em.mkVar("name_init", string);
        Expr mail_init = em.mkVar("mail_init", string);
        Expr callOrigin1 = em.mkVar("callOrigin1", em.mkArrayType(callId, option_txIdDt));
        Expr invocationCalls1 = em.mkVar("invocationCalls1", em.mkArrayType(invocationId, callIdSet));
        Expr transactionOrigin1 = em.mkVar("transactionOrigin1", em.mkArrayType(txId, option_invocationIdDt));
        Expr calls1 = em.mkVar("calls1", em.mkArrayType(callId, callInfoDt));
        Expr tx = em.mkVar("tx", txId);
        Expr bound_u5 = em.mkBoundVar("bound_u5", userId);
        Expr bound_i3 = em.mkBoundVar("bound_i3", invocationId);
        Expr bound_c6 = em.mkBoundVar("bound_c6", callId);
        assertFormula(smt, "e633", em.mkExpr(Kind.FORALL, em.mkExpr(Kind.BOUND_VAR_LIST, bound_u5), em.mkExpr(Kind.FORALL, em.mkExpr(Kind.BOUND_VAR_LIST, bound_i3), em.mkExpr(Kind.IMPLIES, em.mkExpr(Kind.AND, em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, invocationOp, currentInvocation, em.mkExpr(Kind.APPLY_CONSTRUCTOR, registerUser1.getConstructor(), vector(mail_init, name_init))), bound_i3), em.mkExpr(Kind.APPLY_CONSTRUCTOR, removeUser1.getConstructor(), vector(bound_u5))), em.mkExpr(Kind.NOT, em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, invocationRes, bound_i3), em.mkExpr(Kind.APPLY_CONSTRUCTOR, noResult1.getConstructor(), vector())))), em.mkExpr(Kind.EXISTS, em.mkExpr(Kind.BOUND_VAR_LIST, bound_c6), em.mkExpr(Kind.AND, em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.ITE, em.mkExpr(Kind.APPLY_TESTER, none_txId1.getTester(), em.mkExpr(Kind.SELECT, callOrigin1, bound_c6)), em.mkExpr(Kind.APPLY_CONSTRUCTOR, none_invocationId1.getConstructor()), em.mkExpr(Kind.SELECT, transactionOrigin1, em.mkExpr(Kind.APPLY_SELECTOR_TOTAL, some_txId.getSelector("Some_TxId_value"), em.mkExpr(Kind.SELECT, callOrigin1, bound_c6)))), em.mkExpr(Kind.APPLY_CONSTRUCTOR, someInvocationId1.getConstructor(), bound_i3)), em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, calls1, bound_c6), em.mkExpr(Kind.APPLY_CONSTRUCTOR, user_delete1.getConstructor(), vector(bound_u5)))))))));
        Expr u2 = em.mkVar("u2", userId);
        Expr c0 = em.mkVar("c0", callId);
        assertFormula(smt, "e704", em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, calls1, c0), em.mkExpr(Kind.APPLY_CONSTRUCTOR, no_call1.getConstructor())));
        Expr c1 = em.mkVar("c1", callId);
        assertFormula(smt, "e713", em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, calls1, c0, em.mkExpr(Kind.APPLY_CONSTRUCTOR, user_name_assign1.getConstructor(), vector(name_init, u2))), c1), em.mkExpr(Kind.APPLY_CONSTRUCTOR, no_call1.getConstructor())));
        Expr bound_i4 = em.mkBoundVar("bound_i4", invocationId);
        Expr bound_c7 = em.mkBoundVar("bound_c7", callId);
        Expr bound_tx8 = em.mkBoundVar("bound_tx8", txId);
        assertFormula(smt, "e745", em.mkExpr(Kind.FORALL, em.mkExpr(Kind.BOUND_VAR_LIST, bound_i4), em.mkExpr(Kind.FORALL, em.mkExpr(Kind.BOUND_VAR_LIST, bound_c7), em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.MEMBER, bound_c7, em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, invocationCalls1, currentInvocation, em.mkExpr(Kind.INSERT, vector(em.mkExpr(Kind.SINGLETON, c1), c0))), bound_i4)), em.mkExpr(Kind.EXISTS, em.mkExpr(Kind.BOUND_VAR_LIST, bound_tx8), em.mkExpr(Kind.AND, em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, em.mkExpr(Kind.STORE, callOrigin1, c0, em.mkExpr(Kind.APPLY_CONSTRUCTOR, some_txId.getConstructor(), tx)), c1, em.mkExpr(Kind.APPLY_CONSTRUCTOR, some_txId.getConstructor(), tx)), bound_c7), em.mkExpr(Kind.APPLY_CONSTRUCTOR, some_txId.getConstructor(), bound_tx8)), em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, transactionOrigin1, tx, em.mkExpr(Kind.APPLY_CONSTRUCTOR, someInvocationId1.getConstructor(), currentInvocation)), bound_tx8), em.mkExpr(Kind.APPLY_CONSTRUCTOR, someInvocationId1.getConstructor(), bound_i4))))))));
        Expr i1 = em.mkVar("i1", invocationId);
        Expr bound_c11 = em.mkBoundVar("bound_c11", callId);
        assertFormula(smt, "e988", em.mkExpr(Kind.NOT, em.mkExpr(Kind.IMPLIES, em.mkExpr(Kind.AND, em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, invocationOp, currentInvocation, em.mkExpr(Kind.APPLY_CONSTRUCTOR, registerUser1.getConstructor(), vector(mail_init, name_init))), i1), em.mkExpr(Kind.APPLY_CONSTRUCTOR, removeUser1.getConstructor(), vector(em.mkVar("u4", userId)))), em.mkExpr(Kind.NOT, em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, invocationRes, i1), em.mkExpr(Kind.APPLY_CONSTRUCTOR, noResult1.getConstructor(), vector())))), em.mkExpr(Kind.EXISTS, em.mkExpr(Kind.BOUND_VAR_LIST, bound_c11), em.mkExpr(Kind.AND, em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.ITE, em.mkExpr(Kind.APPLY_TESTER, none_txId1.getTester(), em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, em.mkExpr(Kind.STORE, callOrigin1, c0, em.mkExpr(Kind.APPLY_CONSTRUCTOR, some_txId.getConstructor(), tx)), c1, em.mkExpr(Kind.APPLY_CONSTRUCTOR, some_txId.getConstructor(), tx)), bound_c11)), em.mkExpr(Kind.APPLY_CONSTRUCTOR, none_invocationId1.getConstructor()), em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, transactionOrigin1, tx, em.mkExpr(Kind.APPLY_CONSTRUCTOR, someInvocationId1.getConstructor(), currentInvocation)), em.mkExpr(Kind.APPLY_SELECTOR_TOTAL, some_txId.getSelector("Some_TxId_value"), em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, em.mkExpr(Kind.STORE, callOrigin1, c0, em.mkExpr(Kind.APPLY_CONSTRUCTOR, some_txId.getConstructor(), tx)), c1, em.mkExpr(Kind.APPLY_CONSTRUCTOR, some_txId.getConstructor(), tx)), bound_c11)))), em.mkExpr(Kind.APPLY_CONSTRUCTOR, someInvocationId1.getConstructor(), i1)), em.mkExpr(Kind.EQUAL, em.mkExpr(Kind.SELECT, em.mkExpr(Kind.STORE, em.mkExpr(Kind.STORE, calls1, c0, em.mkExpr(Kind.APPLY_CONSTRUCTOR, user_name_assign1.getConstructor(), vector(name_init, u2))), c1, em.mkExpr(Kind.APPLY_CONSTRUCTOR, user_mail_assign1.getConstructor(), vector(mail_init, u2))), bound_c11), em.mkExpr(Kind.APPLY_CONSTRUCTOR, user_delete1.getConstructor(), vector(em.mkVar("u4", userId)))))))));


        Result res = smt.checkSat();
        System.out.println("Res = " + res);


//        UnsatCore core = smt.getUnsatCore();
//        JavaIteratorAdapter_UnsatCore it = core.iterator();
//        while (it.hasNext()) {
//            Expr e = it.next();
//            System.out.println("unsat core: " + e);
//        }

        List<Assertion> core = findMinAssertions(assertions);

        System.out.println("core = ");
        if (core != null) {
            for (Assertion s : core) {
                System.out.println("% " + s.name);
                System.out.println(s.formula);
            }
        } else {
            System.out.println("no core (SAT)");
        }
    }

    @NotNull
    private static vectorExpr vector(Expr e251, Expr e694) {
        vectorExpr e977 = vector(e694);
        e977.add(e251);
        return e977;
    }

    @NotNull
    private static vectorExpr vector(Expr e942) {
        vectorExpr e981 = vector();
        e981.add(e942);
        return e981;
    }

    @NotNull
    private static vectorExpr vector() {
        return new vectorExpr();
    }

    private static void assertFormula(SmtEngine smt, String v, Expr e) {
        assertions.put(v, e);
        smt.assertFormula(e);
    }

    private static List<Assertion> findMinAssertions(Map<String, Expr> smt) throws IOException {
        List<Assertion> assertionsS = new ArrayList<>();
        for (Map.Entry<String, Expr> e : smt.entrySet()) {
            Expr expr = e.getValue();
            assertionsS.add(new Assertion(e.getKey(), "ASSERT " + expr + ";\n"));
        }

        return findMinAssertionsH(assertionsS);
    }

    private static List<Assertion> findMinAssertionsH(List<Assertion> a) throws IOException {
        if (assertionsUnsat(a)) {
            List<Assertion> a2 = new ArrayList<>();

            for (int i = 0; i < a.size(); i++) {
                for (int j = 0; j < a.size(); j++) {
                    if (i != j) {
                        a2.add(a.get(j));
                    }
                }
                List<Assertion> r = findMinAssertionsH(a2);
                if (r != null) {
                    return r;
                }
                a2.clear();
            }
            return a;
        } else {
            return null;
        }
    }

    private static boolean assertionsUnsat(Collection<Assertion> assertions) throws IOException {
        StringBuilder sb = new StringBuilder();

        sb.append("\n");
        sb.append("OPTION \"finite-model-find\" TRUE;\n");
        sb.append("OPTION \"produce-models\" TRUE;\n");
        sb.append("      InvocationId: TYPE;\n");
        sb.append(" % cardinality = beth[0]\n");
        sb.append("UserId: TYPE;\n");
        sb.append(" % cardinality = beth[0]\n");
        sb.append("String: TYPE;\n");
        sb.append(" % cardinality = beth[0]\n");
        sb.append("DATATYPE invocationInfo =\n");
        sb.append("   removeUser(id1: UserId)\n");
        sb.append(" | updateMail(id: UserId, newMail: String)\n");
        sb.append(" | registerUser(name: String, mail: String)\n");
        sb.append(" | getUser(id2: UserId)\n");
        sb.append(" | no_invocation\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("% ARRAY InvocationId OF invocationInfo\n");
        sb.append("TxId: TYPE;\n");
        sb.append(" % cardinality = beth[0]\n");
        sb.append("DATATYPE transactionStatus =\n");
        sb.append("   Uncommitted\n");
        sb.append(" | Committed\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("DATATYPE Option_transactionStatus =\n");
        sb.append("   None_transactionStatus\n");
        sb.append(" | Some_transactionStatus(Some_transactionStatus_value: transactionStatus)\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("% ARRAY TxId OF Option_transactionStatus\n");
        sb.append("CallId: TYPE;\n");
        sb.append(" % cardinality = beth[0]\n");
        sb.append("% SET OF CallId\n");
        sb.append("% ARRAY InvocationId OF SET OF CallId\n");
        sb.append("% ARRAY CallId OF SET OF CallId\n");
        sb.append("DATATYPE getUserResult =\n");
        sb.append("   notFound\n");
        sb.append(" | found(name1: String, mail1: String)\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("DATATYPE invocationResult =\n");
        sb.append("   NoResult\n");
        sb.append(" | getUser_res(getUser_res_arg: getUserResult)\n");
        sb.append(" | registerUser_res(registerUser_res_arg: UserId)\n");
        sb.append(" | removeUser_res\n");
        sb.append(" | updateMail_res\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("% ARRAY InvocationId OF invocationResult\n");
        sb.append("DATATYPE Option_TxId =\n");
        sb.append("   None_TxId\n");
        sb.append(" | Some_TxId(Some_TxId_value: TxId)\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("% ARRAY CallId OF Option_TxId\n");
        sb.append("DATATYPE Option_InvocationId =\n");
        sb.append("   None_InvocationId\n");
        sb.append(" | Some_InvocationId(Some_InvocationId_value: InvocationId)\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("% ARRAY TxId OF Option_InvocationId\n");
        sb.append("% BOOLEAN\n");
        sb.append("DATATYPE callInfo =\n");
        sb.append("   queryop_user_mail_get(key5: UserId, result2: String)\n");
        sb.append(" | queryop_user_name_isEqualTo(key4: UserId, other: String, result1: BOOLEAN)\n");
        sb.append(" | user_delete(key2: UserId)\n");
        sb.append(" | queryop_user_mail_isEqualTo(key6: UserId, other1: String, result3: BOOLEAN)\n");
        sb.append(" | user_mail_assign(key1: UserId, value1: String)\n");
        sb.append(" | user_name_assign(key: UserId, value: String)\n");
        sb.append(" | queryop_user_name_get(key3: UserId, result: String)\n");
        sb.append(" | queryop_user_exists(key7: UserId, result4: BOOLEAN)\n");
        sb.append(" | no_call\n");
        sb.append("END;\n");
        sb.append("\n");
        sb.append("% ARRAY CallId OF callInfo\n");
        sb.append("% SET OF TxId\n");
        sb.append("% ARRAY UserId OF Option_InvocationId\n");
        sb.append("transactionOrigin1: ARRAY TxId OF Option_InvocationId;\n");
        sb.append("invocationRes: ARRAY InvocationId OF invocationResult;\n");
        sb.append("generatedIds_UserId1: ARRAY UserId OF Option_InvocationId;\n");
        sb.append("name_init: String;\n");
        sb.append("u1: UserId;\n");
        sb.append("calls1: ARRAY CallId OF callInfo;\n");
        sb.append("u: UserId;\n");
        sb.append("invocationOp: ARRAY InvocationId OF invocationInfo;\n");
        sb.append("callOrigin: ARRAY CallId OF Option_TxId;\n");
        sb.append("u4: UserId;\n");
        sb.append("invocationCalls: ARRAY InvocationId OF SET OF CallId;\n");
        sb.append("calls: ARRAY CallId OF callInfo;\n");
        sb.append("newCalls: SET OF CallId;\n");
        sb.append("mail_init: String;\n");
        sb.append("vis: SET OF CallId;\n");
        sb.append("g1: InvocationId;\n");
        sb.append("transactionOrigin: ARRAY TxId OF Option_InvocationId;\n");
        sb.append("callOrigin1: ARRAY CallId OF Option_TxId;\n");
        sb.append("i: InvocationId;\n");
        sb.append("transactionStatus1: ARRAY TxId OF Option_transactionStatus;\n");
        sb.append("c1: CallId;\n");
        sb.append("transactionStatus: ARRAY TxId OF Option_transactionStatus;\n");
        sb.append("i1: InvocationId;\n");
        sb.append("u3: UserId;\n");
        sb.append("g: InvocationId;\n");
        sb.append("happensBefore1: ARRAY CallId OF SET OF CallId;\n");
        sb.append("r1: InvocationId;\n");
        sb.append("currentInvocation: InvocationId;\n");
        sb.append("invocationCalls1: ARRAY InvocationId OF SET OF CallId;\n");
        sb.append("c0: CallId;\n");
        sb.append("r: InvocationId;\n");
        sb.append("u2: UserId;\n");
        sb.append("tx: TxId;\n");
        sb.append("happensBefore: ARRAY CallId OF SET OF CallId;\n");
        sb.append("newTxns: SET OF TxId;\n");

        for (Assertion assertion : assertions) {
            sb.append(assertion.formula);
            sb.append("\n");
        }


        sb.append("CHECKSAT;");
        Path f = Paths.get("./model/search" + ++count + ".cvc");
        Files.write(f, sb.toString().getBytes(UTF_8));

        ProcessBuilder pb = new ProcessBuilder("cvc4", f.toString());
        Process p = pb.start();
        String output = inputStreamToString(p.getInputStream());
        return output.contains("unsat");
    }

    private static String inputStreamToString(InputStream inputStream) throws IOException {
        try (ByteArrayOutputStream result = new ByteArrayOutputStream()) {
            byte[] buffer = new byte[1024];
            int length;
            while ((length = inputStream.read(buffer)) != -1) {
                result.write(buffer, 0, length);
            }

            return result.toString("utf-8");
        }
    }

    static class Assertion {
        String name;
        String formula;

        Assertion(String name, String formula) {
            this.name = name;
            this.formula = formula;
        }
    }

}