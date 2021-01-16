package test;

import edu.nyu.acsys.CVC4.*;

class Cvc4Test {

    public static void main(String... args) throws InterruptedException {
        System.loadLibrary("cvc4jni");
        ExprManager em = new ExprManager();
        SmtEngine smt = new SmtEngine(em);
        smt.setOption("produce-models", new SExpr(true));
        smt.setOption("finite-model-find", new SExpr(true));
        smt.setOption("sets-ext", new SExpr(true));
        SortType mySort = em.mkSort("mySort");
        Expr x = em.mkBoundVar("x", mySort);
        Expr y = em.mkBoundVar("y", mySort);
        Expr z = em.mkBoundVar("z", mySort);
        vectorExpr dv = new vectorExpr();
        dv.add(x);
        dv.add(y);
        dv.add(z);
        smt.assertFormula(
                em.mkExpr(Kind.EXISTS, em.mkExpr(Kind.BOUND_VAR_LIST, x, y, z),
                        em.mkExpr(Kind.DISTINCT, dv)));

        Result res = smt.checkSat();
        System.out.println("Res = " + res);
        Expr domain = smt.getValue(em.mkNullaryOperator(em.mkSetType(mySort), Kind.UNIVERSE_SET));
        System.out.println("domain =  " + domain);
        Expr domain2 = smt.getValue(em.mkExpr(Kind.COMPLEMENT, domain));
        System.out.println("domain2 =  " + domain2);

    }

}