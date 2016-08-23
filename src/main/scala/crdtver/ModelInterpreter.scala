package crdtver

import java.io.{File, FileReader}
import java.nio.file.{Files, Path}

import resource.managed

import crdtver.parser.{BoogieModelLexer, BoogieModelParser}
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

class ModelInterpreter {


  def load(file: File): Unit = load(file.toPath)

  def load(fileName: String): Unit = {
    load(new File(fileName))
  }

  def load(input: Path): Unit = {
    for (reader <- managed(Files.newBufferedReader(input))) {
      val inStream = new ANTLRInputStream(reader)
      val lex = new BoogieModelLexer(inStream)
      val tokenStream = new CommonTokenStream(lex)
      val parser = new BoogieModelParser(tokenStream)

      val model = parser.model()

      println(s"loaded model: ${model.toStringTree(parser)}")
    }
  }

}
