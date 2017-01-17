package crdtver

import java.io.{FileNotFoundException, InputStream}

import scala.io.Source

object Helper {
  def getResource(name: String): String = {
    val stream: InputStream = getClass.getResourceAsStream(name)
    if (stream == null) {
      throw new FileNotFoundException(s"Resource not found: $name")
    }
    Source.fromInputStream(stream).mkString
  }
}
