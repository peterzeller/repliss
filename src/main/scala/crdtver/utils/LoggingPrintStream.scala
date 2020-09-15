package crdtver.utils

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

class LoggingPrintStream {
  private val bos = new ByteArrayOutputStream()
  val stream = new PrintStream(bos)

  def getBytes: Array[Byte] = {
    stream.flush()
    bos.toByteArray
  }

  def getString: String = new String(getBytes, StandardCharsets.UTF_8)

}

object LoggingPrintStream {

  def capturePrintStream(f: PrintStream => Unit): String = {
    val s = new LoggingPrintStream
    f(s.stream)
    s.getString
  }

}
