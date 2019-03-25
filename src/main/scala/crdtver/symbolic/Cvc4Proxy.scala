package crdtver.symbolic

import java.lang.reflect.{InvocationHandler, Method, Proxy}

import edu.nyu.acsys.CVC4
import edu.nyu.acsys.CVC4.{Datatype, DatatypeConstructor, DatatypeType, Expr, ExprManager, ExprManagerI, SExpr, SmtEngine, SmtEngineI, Type, vectorExpr}

import scala.collection.mutable

object Cvc4Proxy {
  def getDatatypeConstructor(fdt: DatatypeType, str: String): CVC4.DatatypeConstructor = {
    create(fdt.getDatatype.get(str), s"${printArg(fdt)}.getDatatype.get(${printArg(str)})")
  }

  def getDatatype(dt: DatatypeType): Datatype = {
    create(dt.getDatatype, s"${printArg(dt)}.getDatatype()")
  }

  def addConstructor(t: Datatype, c: CVC4.DatatypeConstructor): Unit = {
    t.addConstructor(c)
    log(s"${printArg(t)}.addConstructor(${printArg(c)});")
  }

  def addConstructorArg(cc: CVC4.DatatypeConstructor, name: String, typ: Type): Unit = {
    cc.addArg(name, typ)
    log(s"${printArg(cc)}.addArg(${printArgs(Array(name, typ))});")
  }

  def DatatypeConstructor(name: String): edu.nyu.acsys.CVC4.DatatypeConstructor = {
    create(new edu.nyu.acsys.CVC4.DatatypeConstructor(name), s"new DatatypeConstructor(${printArg(name)})")
  }

  def Datatype(name: String): Datatype =  {
    create(new Datatype(name), s"new Datatype(${printArg(name)})")
  }

  def SExpr(name: String): SExpr =  {
    create(new SExpr(name), s"new SExpr(${printArg(name)})")
  }

  def SExpr(name: Boolean): SExpr =  {
      create(new SExpr(name), s"new SExpr($name)")
    }

  def SExpr(name: Int): SExpr =  {
    create(new SExpr(name), s"new SExpr($name)")
  }


  def getConstructor(z3t: { def getConstructor(str: String): CVC4.DatatypeConstructor }, str: String): Expr = {
    getConstructor(z3t.getConstructor(str))
  }


  def getConstructor(c: CVC4.DatatypeConstructor): Expr = {
    create(c.getConstructor, s"${printArg(c)}.getConstructor()")
  }


  private var exprNames = Map[AnyRef, String]()
  private var i = 0
  private val cmds: mutable.MutableList[String] = new mutable.MutableList[String]()

  def create[T <: AnyRef](creator: => T, exprUsed: String): T = {
    val r = creator
    val v = newVar
    log(s"${r.getClass.getSimpleName} $v = $exprUsed;")
    exprNames += r -> v
    r
  }

  private def log(msg: String): Unit = {
    println(msg)
    cmds += msg
  }

  def printArgs(objects: Array[AnyRef]): String = {
    if (objects == null)
      return ""
    objects.map(o => printArg(o)).mkString(", ")
  }

  private def printArg(o: AnyRef) = {
    exprNames.getOrElse(o, {
      o match {
        case s: String =>
          "\"" + s + "\""
        case k: CVC4.Kind =>
          s"Kind.$k"
        case _=> s"($o :: ${o.getClass})"
      }

    })
  }

  def exprManager(em: ExprManager): ExprManagerI = {
    val handler = new InvocationHandler {
      override def invoke(o: Any, method: Method, objects: Array[AnyRef]): AnyRef = {
        //        log(s"Calling ${method.getName}(${method.getGenericParameterTypes.mkString(", ")})")
        //        val args =
        //          if (objects == null) {
        //            log(s"  --> ${method.getName}(<<null>>)")
        //            Array[AnyRef]()
        //          } else {
        //            objects
        //          }

        val res = method.invoke(em, objects: _*)
        create(res, s"em.${method.getName}(${printArgs(objects)})")
      }

    }
    Proxy.newProxyInstance(
      classOf[ExprManagerI].getClassLoader,
      Array[Class[_]](classOf[ExprManagerI]), handler).asInstanceOf[ExprManagerI]
  }


  private def newVar = {
    val varName = s"e$i"
    i += 1
    varName
  }

  def smtEngine(em: ExprManager): SmtEngineI = {
    val engine = new SmtEngine(em)
    val handler = new InvocationHandler {
      override def invoke(o: Any, method: Method, objects: Array[AnyRef]): AnyRef = {
        //        log(s"Calling ${method.getName}(${method.getGenericParameterTypes.mkString(", ")})")
        //        val args =
        //          if (objects == null) {
        //            log(s"  --> ${method.getName}(<<null>>)")
        //            Array[AnyRef]()
        //          } else {
        //            objects
        //          }
        val res = method.invoke(engine, objects: _*)
        log(s"smt.${method.getName}(${printArgs(objects)});")
        res
      }

    }


    Proxy.newProxyInstance(
      classOf[SmtEngineI].getClassLoader,
      Array[Class[_]](classOf[SmtEngineI]), handler).asInstanceOf[SmtEngineI]
  }


  def toVectorExpr(exprs: Iterable[Expr]): vectorExpr = {
    val r = new vectorExpr()
    val v = newVar
    exprNames += r -> v
    log(s"vectorExpr $v = new vectorExpr();")
    for (e <- exprs) {
      log(s"$v.add(${printArgs(Array(e))})")
      r.add(e)
    }
    r
  }

}
