package crdtver.symbolic

import edu.nyu.acsys.CVC4._

object Cvc4Test {


  def main(args: Array[String]): Unit = {
    System.loadLibrary("cvc4jni")
    println("library path = " + System.getProperty("java.library.path"))
    val c = new ExprManager()
    val smt = new SmtEngine(c)
    smt.setOption("produce-models", new SExpr(true))
    smt.setOption("finite-model-find", new SExpr(true))
    val intSort = c.integerType()

    val f = c.mkVar("f", c.mkFunctionType(c.integerType(), c.integerType()))
    val x = c.mkBoundVar("x", c.integerType())
    val x2 = c.mkBoundVar("x", c.integerType())
    val x3 = c.mkBoundVar("x", c.integerType())

    val a1 = c.mkExpr(Kind.FORALL, c.mkExpr(Kind.BOUND_VAR_LIST, x),
      c.mkExpr(Kind.GT, c.mkExpr(Kind.APPLY_UF, f, x), c.mkConst(new Rational(42))))

    smt.assertFormula(a1)

    val a2 = c.mkExpr(Kind.NOT, c.mkExpr(Kind.FORALL, c.mkExpr(Kind.BOUND_VAR_LIST, x),
          c.mkExpr(Kind.GT, c.mkExpr(Kind.APPLY_UF, f, x), c.mkConst(new Rational(20)))))

    smt.assertFormula(a2)
    println(a1)
    println(a2)

    val res = smt.checkSat()

    println(s"Res = $res")
    println(s"f = ${smt.getValue(f)}")
    println(s"x = ${smt.getValue(x)}")
    println(s"x2 = ${smt.getValue(x2)}")
    println(s"x3 = ${smt.getValue(x2)}")


//    val x = c.mkVar("x", c.integerType())
//    val y = c.mkVar("y", c.integerType())
//    val z = c.mkVar("z", c.integerType())
//
//
//    smt.assertFormula(c.mkExpr(Kind.LT,
//      c.mkExpr(Kind.PLUS, x, y), z))
//
//    smt.assertFormula(c.mkExpr(Kind.GT,
//          c.mkExpr(Kind.PLUS, x, y), z))
//
//    val res = smt.checkSat()
//    println(s"res = $res")
//    println(s"res = ${res.isNull}")
//    println(s"res = ${res.isSat}")
//    println(s"res = ${res.isUnknown}")
//    println(s"res = ${res.isValid}")
//    val res2 = smt.query()
//    println(s"res2 = $res")
//
//    val model: SWIGTYPE_p_CVC4__Model = smt.getModel
//    val xv = smt.getValue(x)
//    val yv = smt.getValue(y)
//    val zv = smt.getValue(z)
//    println(s"x = $xv, y = $yv, z = $zv")
  }

}
