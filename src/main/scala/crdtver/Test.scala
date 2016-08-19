package crdtver

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

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


    val translator = new BoogieTranslation(parser)
    val boogieProg = translator.transformProgram(prog)

    println(s"BOOGIE: $boogieProg")

    val sb = new StringBuilder
    new BoogiePrinter().printProgram(boogieProg, sb)
    println(s"OUT = $sb")

    val boogieOutputFile = Paths.get("test.bpl")
    Files.write(boogieOutputFile, sb.toString().getBytes(StandardCharsets.UTF_8))

    import sys.process._
    val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!

    println("result: ")
    println(boogieResult)



  }


}