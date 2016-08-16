package crdtver

import crdtver.parser.{LangLexer, LangParser}
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}


object Test {

  def main(args: Array[String]): Unit = {

    val input = io.Source.fromFile("examples/userbase.scala").mkString

    println(input)


    val inStream = new ANTLRInputStream(input)
    val lex = new LangLexer(inStream)
    val tokenStream = new CommonTokenStream(lex)
    val parser = new LangParser(tokenStream)

    val prog = parser.program()

    val s = prog.toStringTree(parser)
    println(s"parsed = $s")


  }


}