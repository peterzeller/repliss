package crdtver.symbolic

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util

import com.microsoft.z3.Context

object Z3Proxy {


  def z3Context(): Z3Context = {
    val context = new Z3ContextImpl(new Context())
    val handler = new InvocationHandler {
      override def invoke(o: Any, method: Method, objects: Array[AnyRef]): AnyRef = {
//        println(s"Calling ${method.getName}(${method.getGenericParameterTypes.mkString(", ")})")
//        val args =
//          if (objects == null) {
//            println(s"  --> ${method.getName}(<<null>>)")
//            Array[AnyRef]()
//          } else {
//            objects
//          }
//        println(s"calling ${method.getName}(${util.Arrays.deepToString(objects)})")
        val res = method.invoke(context, objects:_*)
//        println(s"  res = $res")
        res
      }

    }


    Proxy.newProxyInstance(
      classOf[Z3Context].getClassLoader,
      Array[Class[_]](classOf[Z3Context]), handler).asInstanceOf[Z3Context]
  }

}
