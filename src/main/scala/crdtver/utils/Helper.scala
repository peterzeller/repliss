package crdtver.utils

import java.io.{File, FileNotFoundException, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{CopyOption, Files, StandardCopyOption}

import crdtver.Repliss
import crdtver.language.TypedAst
import crdtver.symbolic.smt.Smt
import crdtver.symbolic.{SVal, SymbolicSort}

import scala.io.Source
import scala.util.Using

object Helper {
  def printStacktrace(t: Throwable): String = {
    LoggingPrintStream.capturePrintStream { p => t.printStackTrace(p) }
  }

  /** writes a file atomically */
  def writeFile(resultFile: File, content: String): Unit = {
    val tempFile = Files.createTempFile(resultFile.getParentFile.toPath, resultFile.getName, "")
    Files.write(tempFile, content.getBytes(StandardCharsets.UTF_8))
    Files.move(tempFile, resultFile.toPath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
  }


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

  def readFile(f: File): String = {
    Using(scala.io.Source.fromFile(f))(_.mkString).get
  }

}
