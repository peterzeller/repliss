package crdtver.utils

import java.util

import PrettyPrintDoc._
import crdtver.language.InputAst.{NoSource, ParserRuleSource, SourceTrace}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.IterableHasAsScala

object DebugPrint {


  def debugPrint(x: Any): PrettyPrintDoc.Doc =
    debugPrint2(x)(new util.HashMap())

  private def debugPrint2(x: Any)(implicit visited: util.HashMap[Any, Unit]): PrettyPrintDoc.Doc = {
    if (visited.containsKey(x)) {
      return "#cyclic#"
    }
    visited.put(x, ())

    val res: Doc = x match {
      case s: String =>
        "\"" <> s <> "\""
      case x: Number => x.toString
      case x: Boolean => x.toString
      case (x,y) =>
        "(" <> debugPrint2(x) <> ", " <> debugPrint(y) <> ")"
      case l: List[_] =>
        fc("List", l.map(debugPrint2))
      case l: Set[_] =>
        fc("Set", l.toList.map(debugPrint2))
      case m: Map[_, _] =>
        fc("Map", m.toList.map(e => debugPrint2(e._1) <> " -> " <> debugPrint2(e._2)))
      case l: Iterable[_] =>
        fc(l.getClass.getSimpleName, l.map(debugPrint2).toList)
      case l: java.lang.Iterable[_] =>
        fc(l.getClass.getSimpleName, l.asScala.map(debugPrint2).toList)
      case m: java.util.Map[_, _] =>
        fc("JavaMap", m.entrySet().asScala.toList.map(e => debugPrint2(e.getKey) <> " -> " <> debugPrint2(e.getValue)))
      case p: SourceTrace =>
        "NoSource()"
      case p: Product =>
        val values: List[Doc] =
          for (f <- p.productIterator.toList) yield {
            debugPrint2(f)
          }
        fc(p.productPrefix, values)
      case c: AnyRef =>
        val clazz = c.getClass
        val declaredFields = clazz.getDeclaredFields
        val values: List[Doc] =
          for (f <- declaredFields.reverseIterator.toList) yield {
            f.setAccessible(true)
            val fv = f.get(c)
//            println(s"${"  " * visited.size()} get field ${f.getName} of ${clazz.getSimpleName} -> ${tryPrint(fv)}")
            debugPrint2(fv)
          }

        fc(clazz.getSimpleName, values)
      case null =>
        "null"
      case _ =>
        throw new Exception(s"unhandled case: ${x.getClass} // $x")

    }
    visited.remove(x)
    res
  }

  def tryPrint(x: Any): String =
    try {
      x.toString
    } catch {
      case _: Throwable => "error"
    }

  def fc(name: String, parts: List[Doc]): Doc =
    group(name <> "(" <> nested(2, line <> sep("," <> lineOrSpace, parts)) <> ")")

  case class A(x: Int, y: String)

  def main(args: Array[String]): Unit = {

    val s = debugPrint(List(1, A(42, "Hello"), 3))
    println(s)
  }

}
