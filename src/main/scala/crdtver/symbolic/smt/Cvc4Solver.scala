package crdtver.symbolic.smt

import crdtver.symbolic.Cvc4Proxy
import edu.nyu.acsys.CVC4._

import scala.collection.Set

/**
  *
  */
class Cvc4Solver extends Solver {

  private var variables: Map[String, Type] = Map()
  private var usedVarNames: Set[String] = Set()

  System.loadLibrary("cvc4jni")

  val emIntern: ExprManager = new ExprManager()
  val em: ExprManagerI = Cvc4Proxy.exprManager(emIntern)




  // TODO push pop optimization



  override def check(expression: List[Smt.SmtExpr]): CheckRes = {
    val smt: SmtEngineI = Cvc4Proxy.smtEngine(emIntern)
    smt.setOption("produce-models", Cvc4Proxy.SExpr(true))
    smt.setOption("finite-model-find", Cvc4Proxy.SExpr(true))
    smt.setOption("e-matching", Cvc4Proxy.SExpr(true))
    smt.setOption("incremental", Cvc4Proxy.SExpr(true))
    smt.setOption("tlimit", Cvc4Proxy.SExpr(30000))
    smt.setOption("produce-assertions", Cvc4Proxy.SExpr(true))
    smt.setOption("output-language", Cvc4Proxy.SExpr("cvc4")); // Set the output-language to CVC's
    smt.setOption("default-dag-thresh", Cvc4Proxy.SExpr(0)); //Disable dagifying the output
    for (e <- expression) {
      smt.assertFormula(translateExpr(e))
    }
    val res = smt.checkSat()
    val sat: Result.Sat = res.isSat
    if (sat == Result.Sat.UNSAT) {
      Unsatisfiable()
    } else if (sat == Result.Sat.SAT_UNKNOWN) {
      Unknown()
    } else {
      new Satisfiable {

        override def getModel: Model = new Model {

          override def toString: String = {
            "satisfiable model"
          }

          override def eval(expr: Smt.SmtExpr, bool: Boolean): Smt.SmtExpr = {
            val r = smt.getValue(translateExpr(expr))
            readExpr(r)
          }
        }
      }
    }
  }

  def translateExpr(e: Smt.SmtExpr): Expr = ???

  def readExpr(r: Expr): Smt.SmtExpr = ???

}

