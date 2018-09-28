package crdtver.symbolic

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.file.Files

import com.microsoft.z3._

object Z3Test {



  def main(args: Array[String]): Unit = {
//    new Z3Loader().loadZ3Library()
    println("library path = " + System.getProperty("java.library.path"))
    val c = new Context()
    val intSort = c.getIntSort

    val x = c.mkFuncDecl(c.mkSymbol("x"), Array[Sort](), intSort)
    val y = c.mkFuncDecl(c.mkSymbol("y"), Array[Sort](), intSort)
    val z = c.mkFuncDecl(c.mkSymbol("z"), Array[Sort](), intSort)

    val solver = c.mkSolver()
    solver.add(c.mkLt(c.mkAdd(c.mkConst(x).asInstanceOf[ArithExpr], c.mkConst(y).asInstanceOf[ArithExpr]), c.mkConst(z).asInstanceOf[ArithExpr]))

    val res = solver.check()
    println(s"res = $res")
    val model = solver.getModel
    val xv = model.eval(c.mkConst(x), true)
    val yv = model.eval(c.mkConst(y), true)
    val zv = model.eval(c.mkConst(z), true)
    println(s"x = $xv, y = $yv, z = $zv")


  }

}
