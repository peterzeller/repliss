package crdtver

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import crdtver.InputAst.InProgram
import crdtver.parser.{LangLexer, LangParser}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime._


object Test {

  def main(args: Array[String]): Unit = {

    val inputFileStr: String = if (args.length == 0) {
      "examples/userbase.scala"
    } else {
      args(0)
    }
    val inputFile = new File(inputFileStr)
    if (!inputFile.exists()) {
      println(s"Input file $inputFileStr not found.")
      return
    }
    println(s"Reading input $inputFileStr")

    val input = io.Source.fromFile(inputFile).mkString

    val inStream = new ANTLRInputStream(input)
    val lex = new LangLexer(inStream)
    val tokenStream = new CommonTokenStream(lex)
    val parser = new LangParser(tokenStream)

    var errorCount = 0
    val errorListener = new ANTLRErrorListener {

      override def reportContextSensitivity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {
      }

      override def reportAmbiguity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: util.BitSet, configs: ATNConfigSet): Unit = {
      }

      override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet, configs: ATNConfigSet): Unit = {
      }

      override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
        errorCount += 1
        println(s"Error $errorCount line $line:$charPositionInLine $msg")
      }

    }

    lex.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)


    val prog = parser.program()

    if (errorCount > 0) {
      println(s"There were $errorCount parser errors.")
      return
    }



    val s = prog.toStringTree(parser)

    val inputProg = InputAst.transformProgram(prog)

//    println(s"input prog = $inputProg")

    val typedInputProg: InProgram = try {
      val typer = new Typer()
      typer.checkProgram(inputProg)
    } catch {
      case err: Typer.TypeErrorException =>
        println(err.getMessage)
        return
    }

//    println(s"typed input prog = $inputProg")


    val translator = new WhyTranslation(parser)
    val whyProg = translator.transformProgram(typedInputProg)

//    println(s"BOOGIE: $boogieProg")

    val printer: WhyPrinter = new WhyPrinter()
    val printedBoogie = printer.printProgram(whyProg)
//    println(s"OUT = $sb")


    new File("model").mkdirs()

    val boogieOutputFile = Paths.get("model/temp.mlw")
    Files.write(boogieOutputFile, printedBoogie.getBytes(StandardCharsets.UTF_8))

    println("Starting why3")

    import sys.process._
    //val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!
    val why3Result: String = "why3 prove -P z3 model/temp.mlw".!!
//    val boogieResult: String = "boogie model/test.bpl /errorLimit:1 /timeLimit:5 ".!!


    Files.write(Paths.get("model/why3Output.txt"), why3Result.getBytes(StandardCharsets.UTF_8))

    println("--- BEGIN Output --------------")
    println(why3Result)
    println("--- END Output --------------")


//    val boogieOutputParser = new BoogieOutputParser()
//    boogieOutputParser.parse(boogieResult)
//    boogieOutputParser.printErrors(printer.sourceMap, input)
//    if (boogieOutputParser.errorCount() == 0) {
//      println("Verification successful!")
//    } else if (boogieOutputParser.errorCount() == 1) {
//      println(s"There was 1 verification error")
//    } else {
//      println(s"There were ${boogieOutputParser.errorCount()} verification errors")
//    }


  }




}