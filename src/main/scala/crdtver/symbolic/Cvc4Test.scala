package crdtver.symbolic

import edu.nyu.acsys.CVC4._

object Cvc4Test {


  def main(args: Array[String]): Unit = {
    System.loadLibrary("cvc4jni")
    //    new Z3Loader().loadZ3Library()
    println("library path = " + System.getProperty("java.library.path"))
    val c = new ExprManager()
    val smt = new SmtEngine(c)
    smt.setOption("produce-models", new SExpr(true))
    val intSort = c.integerType()

    val x = c.mkVar("x", c.integerType())
    val y = c.mkVar("y", c.integerType())
    val z = c.mkVar("z", c.integerType())


    smt.assertFormula(c.mkExpr(Kind.LT,
      c.mkExpr(Kind.PLUS, x, y), z))

    val res = smt.checkSat()
    println(s"res = $res")
    val res2 = smt.query()
    println(s"res2 = $res")

    val model: SWIGTYPE_p_CVC4__Model = smt.getModel
    val xv = smt.getValue(x)
    val yv = smt.getValue(y)
    val zv = smt.getValue(z)
    println(s"x = $xv, y = $yv, z = $zv")
  }

}
