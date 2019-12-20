package crdtver.utils

import java.io.{FileNotFoundException, InputStream}

import crdtver.language.TypedAst
import crdtver.symbolic.{SVal, SymbolicSort}

import scala.io.Source

object Helper {

  private class UnexpectedCaseException(x: Any) extends Exception(s"Unexpeceted case: $x")


  def unexpected(x: Any): Nothing =
    throw new UnexpectedCaseException(x)

  def getResource(name: String): String = {
    val stream: InputStream = getClass.getResourceAsStream(name)
    if (stream == null) {
      throw new FileNotFoundException(s"Resource not found: $name")
    }
    Source.fromInputStream(stream).mkString
  }
}
