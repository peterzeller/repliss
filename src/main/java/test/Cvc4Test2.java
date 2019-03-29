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

        SExpr e0 = new SExpr(true);
        smt.setOption("produce-models", e0);
        SExpr e1 = new SExpr(true);
        smt.setOption("finite-model-find", e1);
        SExpr e2 = new SExpr(true);
        smt.setOption("e-matching", e2);
        SExpr e3 = new SExpr(true);
        smt.setOption("incremental", e3);
        SExpr e4 = new SExpr(30000);
        smt.setOption("tlimit", e4);
        SExpr e5 = new SExpr(true);
        smt.setOption("produce-assertions", e5);
        SExpr e6 = new SExpr("cvc4");
        smt.setOption("output-language", e6);
        SExpr e7 = new SExpr(0);
        smt.setOption("default-dag-thresh", e7);
        smt.setOption("produce-unsat-cores", new SExpr(true));
        SortType e8 = em.mkSort("InvocationId");
        DatatypeConstructor e9 = new DatatypeConstructor("removeUser");
        SortType e10 = em.mkSort("UserId");
        e9.addArg("id1", e10);
        DatatypeConstructor e11 = new DatatypeConstructor("updateMail");
        e11.addArg("id", e10);
        SortType e12 = em.mkSort("String");
        e11.addArg("newMail", e12);
        DatatypeConstructor e13 = new DatatypeConstructor("registerUser");
        e13.addArg("name", e12);
        e13.addArg("mail", e12);
        DatatypeConstructor e14 = new DatatypeConstructor("getUser");
        e14.addArg("id2", e10);
        DatatypeConstructor e15 = new DatatypeConstructor("no_invocation");
        Datatype e16 = new Datatype("invocationInfo");
        e16.addConstructor(e9);
        e16.addConstructor(e11);
        e16.addConstructor(e13);
        e16.addConstructor(e14);
        e16.addConstructor(e15);
        DatatypeType e17 = em.mkDatatypeType(e16);
        DatatypeConstructor e18 = e17.getDatatype().get("removeUser");
        DatatypeConstructor e20 = e17.getDatatype().get("registerUser");
        ArrayType e23 = em.mkArrayType(e8, e17);
        Expr e24 = em.mkVar("invocationOp", e23);
        Expr e25 = em.mkVar("currentInvocation", e8);
        SortType e30 = em.mkSort("TxId");
        DatatypeConstructor e33 = new DatatypeConstructor("Uncommitted");
        DatatypeConstructor e34 = new DatatypeConstructor("Committed");
        Datatype e35 = new Datatype("transactionStatus");
        e35.addConstructor(e33);
        e35.addConstructor(e34);
        DatatypeType e36 = em.mkDatatypeType(e35);
        Datatype e39 = new Datatype("Option_transactionStatus");
        DatatypeConstructor e40 = new DatatypeConstructor("None_transactionStatus");
        DatatypeConstructor e41 = new DatatypeConstructor("Some_transactionStatus");
        e41.addArg("Some_transactionStatus_value", e36);
        e39.addConstructor(e40);
        e39.addConstructor(e41);
        SortType e72 = em.mkSort("CallId");
        SetType e73 = em.mkSetType(e72);
        ArrayType e74 = em.mkArrayType(e8, e73);
        DatatypeConstructor e103 = new DatatypeConstructor("NoResult");
        DatatypeConstructor e104 = new DatatypeConstructor("getUser_res");
        DatatypeConstructor e105 = new DatatypeConstructor("notFound");
        DatatypeConstructor e106 = new DatatypeConstructor("found");
        e106.addArg("name1", e12);
        e106.addArg("mail1", e12);
        Datatype e107 = new Datatype("getUserResult");
        e107.addConstructor(e105);
        e107.addConstructor(e106);
        DatatypeType e108 = em.mkDatatypeType(e107);
        e104.addArg("getUser_res_arg", e108);
        DatatypeConstructor e111 = new DatatypeConstructor("registerUser_res");
        e111.addArg("registerUser_res_arg", e10);
        DatatypeConstructor e112 = new DatatypeConstructor("removeUser_res");
        DatatypeConstructor e113 = new DatatypeConstructor("updateMail_res");
        Datatype e114 = new Datatype("invocationResult");
        e114.addConstructor(e103);
        e114.addConstructor(e104);
        e114.addConstructor(e111);
        e114.addConstructor(e112);
        e114.addConstructor(e113);
        DatatypeType e115 = em.mkDatatypeType(e114);
        DatatypeConstructor e116 = e115.getDatatype().get("NoResult");
        ArrayType e121 = em.mkArrayType(e8, e115);
        Expr e122 = em.mkVar("invocationRes", e121);
        Datatype e153 = new Datatype("Option_TxId");
        DatatypeConstructor e154 = new DatatypeConstructor("None_TxId");
        DatatypeConstructor e155 = new DatatypeConstructor("Some_TxId");
        e155.addArg("Some_TxId_value", e30);
        e153.addConstructor(e154);
        e153.addConstructor(e155);
        DatatypeType e156 = em.mkDatatypeType(e153);
        DatatypeConstructor e157 = e156.getDatatype().get("None_TxId");
        DatatypeConstructor e158 = e156.getDatatype().get("Some_TxId");
        ArrayType e159 = em.mkArrayType(e72, e156);
        Datatype e162 = new Datatype("Option_InvocationId");
        DatatypeConstructor e163 = new DatatypeConstructor("None_InvocationId");
        DatatypeConstructor e164 = new DatatypeConstructor("Some_InvocationId");
        e164.addArg("Some_InvocationId_value", e8);
        e162.addConstructor(e163);
        e162.addConstructor(e164);
        DatatypeType e165 = em.mkDatatypeType(e162);
        DatatypeConstructor e166 = e165.getDatatype().get("None_InvocationId");
        DatatypeConstructor e167 = e165.getDatatype().get("Some_InvocationId");
        ArrayType e172 = em.mkArrayType(e30, e165);
        DatatypeConstructor e181 = new DatatypeConstructor("queryop_user_mail_get");
        e181.addArg("key5", e10);
        e181.addArg("result2", e12);
        DatatypeConstructor e182 = new DatatypeConstructor("queryop_user_name_isEqualTo");
        e182.addArg("key4", e10);
        e182.addArg("other", e12);
        BooleanType e183 = em.booleanType();
        e182.addArg("result1", e183);
        DatatypeConstructor e184 = new DatatypeConstructor("user_delete");
        e184.addArg("key2", e10);
        DatatypeConstructor e185 = new DatatypeConstructor("queryop_user_mail_isEqualTo");
        e185.addArg("key6", e10);
        e185.addArg("other1", e12);
        e185.addArg("result3", e183);
        DatatypeConstructor e186 = new DatatypeConstructor("user_mail_assign");
        e186.addArg("key1", e10);
        e186.addArg("value1", e12);
        DatatypeConstructor e187 = new DatatypeConstructor("user_name_assign");
        e187.addArg("key", e10);
        e187.addArg("value", e12);
        DatatypeConstructor e188 = new DatatypeConstructor("queryop_user_name_get");
        e188.addArg("key3", e10);
        e188.addArg("result", e12);
        DatatypeConstructor e189 = new DatatypeConstructor("queryop_user_exists");
        e189.addArg("key7", e10);
        e189.addArg("result4", e183);
        DatatypeConstructor e190 = new DatatypeConstructor("no_call");
        Datatype e191 = new Datatype("callInfo");
        e191.addConstructor(e181);
        e191.addConstructor(e182);
        e191.addConstructor(e184);
        e191.addConstructor(e185);
        e191.addConstructor(e186);
        e191.addConstructor(e187);
        e191.addConstructor(e188);
        e191.addConstructor(e189);
        e191.addConstructor(e190);
        DatatypeType e192 = em.mkDatatypeType(e191);
        DatatypeConstructor e195 = e192.getDatatype().get("user_delete");
        DatatypeConstructor e197 = e192.getDatatype().get("user_mail_assign");
        DatatypeConstructor e198 = e192.getDatatype().get("user_name_assign");
        DatatypeConstructor e201 = e192.getDatatype().get("no_call");
        ArrayType e202 = em.mkArrayType(e72, e192);
        Expr e250 = em.mkVar("name_init", e12);
        Expr e251 = em.mkVar("mail_init", e12);
        vectorExpr e252 = vector(e251, e250);
        Expr e253 = e20.getConstructor();
        Expr e254 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e253, e252);
        Expr e255 = em.mkExpr(Kind.STORE, e24, e25, e254);
        vectorExpr e311 = vector();
        Expr e312 = e116.getConstructor();
        Expr e313 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e312, e311);
        Expr e381 = em.mkVar("callOrigin1", e159);
        Expr e404 = em.mkVar("invocationCalls1", e74);
        Expr e413 = em.mkVar("transactionOrigin1", e172);
        Expr e429 = em.mkVar("calls1", e202);
        Expr e515 = em.mkVar("tx", e30);
        Expr e590 = em.mkBoundVar("bound_u5", e10);
        Expr e591 = em.mkExpr(Kind.BOUND_VAR_LIST, e590);
        Expr e592 = em.mkBoundVar("bound_i3", e8);
        Expr e593 = em.mkExpr(Kind.BOUND_VAR_LIST, e592);
        vectorExpr e594 = vector(e251, e250);
        Expr e595 = e20.getConstructor();
        Expr e596 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e595, e594);
        Expr e597 = em.mkExpr(Kind.STORE, e24, e25, e596);
        Expr e598 = em.mkExpr(Kind.SELECT, e597, e592);
        vectorExpr e599 = vector(e590);
        Expr e600 = e18.getConstructor();
        Expr e601 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e600, e599);
        Expr e602 = em.mkExpr(Kind.EQUAL, e598, e601);
        Expr e603 = em.mkExpr(Kind.SELECT, e122, e592);
        vectorExpr e604 = vector();
        Expr e605 = e116.getConstructor();
        Expr e606 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e605, e604);
        Expr e607 = em.mkExpr(Kind.EQUAL, e603, e606);
        Expr e608 = em.mkExpr(Kind.NOT, e607);
        Expr e609 = em.mkExpr(Kind.AND, e602, e608);
        Expr e610 = em.mkBoundVar("bound_c6", e72);
        Expr e611 = em.mkExpr(Kind.BOUND_VAR_LIST, e610);
        Expr e612 = em.mkExpr(Kind.SELECT, e381, e610);
        Expr e613 = e166.getConstructor();
        Expr e614 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e613);
        Expr e615 = e158.getSelector("Some_TxId_value");
        Expr e616 = em.mkExpr(Kind.APPLY_SELECTOR_TOTAL, e615, e612);
        Expr e617 = em.mkExpr(Kind.SELECT, e413, e616);
        Expr e618 = e157.getTester();
        Expr e619 = em.mkExpr(Kind.APPLY_TESTER, e618, e612);
        Expr e620 = em.mkExpr(Kind.ITE, e619, e614, e617);
        Expr e621 = e167.getConstructor();
        Expr e622 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e621, e592);
        Expr e623 = em.mkExpr(Kind.EQUAL, e620, e622);
        Expr e624 = em.mkExpr(Kind.SELECT, e429, e610);
        vectorExpr e625 = vector(e590);
        Expr e626 = e195.getConstructor();
        Expr e627 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e626, e625);
        Expr e628 = em.mkExpr(Kind.EQUAL, e624, e627);
        Expr e629 = em.mkExpr(Kind.AND, e623, e628);
        Expr e630 = em.mkExpr(Kind.EXISTS, e611, e629);
        Expr e631 = em.mkExpr(Kind.IMPLIES, e609, e630);
        Expr e632 = em.mkExpr(Kind.FORALL, e593, e631);
        Expr e633 = em.mkExpr(Kind.FORALL, e591, e632);
        assertFormula(smt, "e633", e633);
        Expr e694 = em.mkVar("u2", e10);
        Expr e700 = em.mkVar("c0", e72);
        Expr e701 = em.mkExpr(Kind.SELECT, e429, e700);
        Expr e702 = e201.getConstructor();
        Expr e703 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e702);
        Expr e704 = em.mkExpr(Kind.EQUAL, e701, e703);
        assertFormula(smt, "e704", e704);
        Expr e705 = em.mkVar("c1", e72);
        Expr e708 = e198.getConstructor();
        vectorExpr e709 = vector(e250, e694);
        Expr e710 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e708, e709);
        Expr e711 = em.mkExpr(Kind.STORE, e429, e700, e710);
        Expr e712 = em.mkExpr(Kind.SELECT, e711, e705);
        Expr e713 = em.mkExpr(Kind.EQUAL, e712, e703);
        assertFormula(smt, "e713", e713);
        Expr e714 = em.mkBoundVar("bound_i4", e8);
        Expr e715 = em.mkExpr(Kind.BOUND_VAR_LIST, e714);
        Expr e716 = em.mkBoundVar("bound_c7", e72);
        Expr e717 = em.mkExpr(Kind.BOUND_VAR_LIST, e716);
        Expr e718 = em.mkExpr(Kind.SINGLETON, e705);
        vectorExpr e719 = vector(e718, e700);
        Expr e720 = em.mkExpr(Kind.INSERT, e719);
        Expr e721 = em.mkExpr(Kind.STORE, e404, e25, e720);
        Expr e722 = em.mkExpr(Kind.SELECT, e721, e714);
        Expr e723 = em.mkExpr(Kind.MEMBER, e716, e722);
        Expr e724 = em.mkBoundVar("bound_tx8", e30);
        Expr e725 = em.mkExpr(Kind.BOUND_VAR_LIST, e724);
        Expr e726 = e158.getConstructor();
        Expr e727 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e726, e515);
        Expr e728 = em.mkExpr(Kind.STORE, e381, e700, e727);
        Expr e729 = em.mkExpr(Kind.STORE, e728, e705, e727);
        Expr e730 = em.mkExpr(Kind.SELECT, e729, e716);
        Expr e731 = e158.getConstructor();
        Expr e732 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e731, e724);
        Expr e733 = em.mkExpr(Kind.EQUAL, e730, e732);
        Expr e734 = e167.getConstructor();
        Expr e735 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e734, e25);
        Expr e736 = em.mkExpr(Kind.STORE, e413, e515, e735);
        Expr e737 = em.mkExpr(Kind.SELECT, e736, e724);
        Expr e738 = e167.getConstructor();
        Expr e739 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e738, e714);
        Expr e740 = em.mkExpr(Kind.EQUAL, e737, e739);
        Expr e741 = em.mkExpr(Kind.AND, e733, e740);
        Expr e742 = em.mkExpr(Kind.EXISTS, e725, e741);
        Expr e743 = em.mkExpr(Kind.EQUAL, e723, e742);
        Expr e744 = em.mkExpr(Kind.FORALL, e717, e743);
        Expr e745 = em.mkExpr(Kind.FORALL, e715, e744);
        assertFormula(smt, "e745", e745);
        Expr e940 = em.mkVar("i1", e8);
        Expr e941 = em.mkExpr(Kind.SELECT, e255, e940);
        Expr e942 = em.mkVar("u4", e10);
        vectorExpr e943 = vector(e942);
        Expr e944 = e18.getConstructor();
        Expr e945 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e944, e943);
        Expr e946 = em.mkExpr(Kind.EQUAL, e941, e945);
        Expr e947 = em.mkExpr(Kind.SELECT, e122, e940);
        Expr e948 = em.mkExpr(Kind.EQUAL, e947, e313);
        Expr e949 = em.mkExpr(Kind.NOT, e948);
        Expr e950 = em.mkExpr(Kind.AND, e946, e949);
        Expr e951 = em.mkBoundVar("bound_c11", e72);
        Expr e952 = em.mkExpr(Kind.BOUND_VAR_LIST, e951);
        Expr e953 = e158.getConstructor();
        Expr e954 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e953, e515);
        Expr e955 = em.mkExpr(Kind.STORE, e381, e700, e954);
        Expr e956 = em.mkExpr(Kind.STORE, e955, e705, e954);
        Expr e957 = em.mkExpr(Kind.SELECT, e956, e951);
        Expr e958 = e166.getConstructor();
        Expr e959 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e958);
        Expr e960 = e158.getSelector("Some_TxId_value");
        Expr e961 = em.mkExpr(Kind.APPLY_SELECTOR_TOTAL, e960, e957);
        Expr e962 = e167.getConstructor();
        Expr e963 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e962, e25);
        Expr e964 = em.mkExpr(Kind.STORE, e413, e515, e963);
        Expr e965 = em.mkExpr(Kind.SELECT, e964, e961);
        Expr e966 = e157.getTester();
        Expr e967 = em.mkExpr(Kind.APPLY_TESTER, e966, e957);
        Expr e968 = em.mkExpr(Kind.ITE, e967, e959, e965);
        Expr e969 = e167.getConstructor();
        Expr e970 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e969, e940);
        Expr e971 = em.mkExpr(Kind.EQUAL, e968, e970);
        Expr e972 = e198.getConstructor();
        vectorExpr e973 = vector(e250, e694);
        Expr e974 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e972, e973);
        Expr e975 = em.mkExpr(Kind.STORE, e429, e700, e974);
        Expr e976 = e197.getConstructor();
        vectorExpr e977 = vector(e251, e694);
        Expr e978 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e976, e977);
        Expr e979 = em.mkExpr(Kind.STORE, e975, e705, e978);
        Expr e980 = em.mkExpr(Kind.SELECT, e979, e951);
        vectorExpr e981 = vector(e942);
        Expr e982 = e195.getConstructor();
        Expr e983 = em.mkExpr(Kind.APPLY_CONSTRUCTOR, e982, e981);
        assertFormula(smt, "e988", em.mkExpr(Kind.NOT, em.mkExpr(Kind.IMPLIES, e950, em.mkExpr(Kind.EXISTS, e952, em.mkExpr(Kind.AND, e971, em.mkExpr(Kind.EQUAL, e980, e983))))));


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

            return result.toString(UTF_8);
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