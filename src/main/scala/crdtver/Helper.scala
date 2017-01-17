package crdtver

import java.io.InputStream

import scala.io.Source

object Helper {
  def getResource(name: String): String = {
    val stream: InputStream = getClass.getResourceAsStream(name)
    Source.fromInputStream(stream).mkString
  }
}
