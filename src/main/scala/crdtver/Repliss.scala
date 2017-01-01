package crdtver

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import crdtver.InputAst.{AstElem, InProgram}
import crdtver.Typer.TypeErrorException
import crdtver.WhyAst.Module
import crdtver.parser.{LangLexer, LangParser}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime._

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.matching.Regex


object Repliss {

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

    val res = for (
      inputProg <- parseFile(inputFile);
      typedInputProg <- typecheck(inputProg);
      whyProg = translateProg(typedInputProg);
      why3Result <- checkWhyModule(whyProg)
    ) yield {
      why3Result
    }
//
//    val inputProg = parseFile(inputFile)
//
//    //    println(s"input prog = $inputProg")
//
//    val typedInputProg: InProgram = typecheck(inputProg)
//
//    //    println(s"typed input prog = $inputProg")
//
//
//    val whyProg: Module = translateProg(typedInputProg)
//
//    //    println(s"BOOGIE: $boogieProg")
//
//    val why3Result: String = checkWhyModule(whyProg)
//    //    val boogieResult: String = "boogie model/test.bpl /errorLimit:1 /timeLimit:5 ".!!
//
//
//    Files.write(Paths.get("model/why3Output.txt"), why3Result.getBytes(StandardCharsets.UTF_8))

    println("--- BEGIN Output --------------")
    for (r <- res; pr <- r) {
      println(pr)
    }


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


  private def checkWhyModule(whyProg: Module): Result[List[Why3Result]] = {
    val printedWhycode: String = printWhyProg(whyProg)
    //    println(s"OUT = $sb")


    val why3Result = checkWhy3code(printedWhycode)
    NormalResult(why3Result)
  }

  private def checkWhy3code(printedWhycode: String): List[Why3Result] = {
    new File("model").mkdirs()

    val boogieOutputFile = Paths.get("model/temp.mlw")
    Files.write(boogieOutputFile, printedWhycode.getBytes(StandardCharsets.UTF_8))

    println("Starting why3")

    import sys.process._
    //val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!
    val why3Result: String = "why3 prove -P z3 model/temp.mlw".!!

    val resultRegexp: Regex = "([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) : ([^ ]+) \\(([0-9.]+)s\\)".r

    for (line <- why3Result.split("\n").toList) yield {
      val resultRegexp(file,module,t,proc,resStr,timeStr) = line
      val res = resStr match {
        case "Valid" => Valid()
        case "Timeout" => Timeout()
        case _ => Unknown(resStr)
      }
      val time = timeStr.toDouble
      Why3Result(proc, res, time)
    }
  }

  case class Why3Result(
    proc: String,
    res: Why3VerificationResult,
    time: Double
  )

  sealed abstract class Why3VerificationResult

  case class Valid() extends Why3VerificationResult
  case class Timeout() extends Why3VerificationResult
  case class Unknown(s: String) extends Why3VerificationResult



  private def printWhyProg(whyProg: Module) = {
    val printer: WhyPrinter = new WhyPrinter()
    val printed = printer.printProgram(whyProg)
    printed
  }

  private def translateProg(typedInputProg: InProgram): Module = {
    val translator = new WhyTranslation()
    val whyProg = translator.transformProgram(typedInputProg)
    whyProg
  }

  private def typecheck(inputProg: InProgram): Result[InProgram] = {
    val typer = new Typer()
    NormalResult(typer.checkProgram(inputProg))
    // TODO errorhandling
  }

  private def parseFile(inputFile: File): Result[InProgram] = {
    println(s"Reading input $inputFile")

    val input = io.Source.fromFile(inputFile).mkString

    parseInput(input)
  }

  private def parseInput(input: String): Result[InProgram] = {
    val inStream = new ANTLRInputStream(input)
    val lex = new LangLexer(inStream)
    val tokenStream = new CommonTokenStream(lex)
    val parser = new LangParser(tokenStream)
    var errors = List[Error]()

    val errorListener = new ANTLRErrorListener {

      override def reportContextSensitivity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {
      }

      override def reportAmbiguity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: util.BitSet, configs: ATNConfigSet): Unit = {
      }

      override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet, configs: ATNConfigSet): Unit = {
      }

      override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: scala.Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
        val pos = SourcePosition(line, charPositionInLine)
        errors = errors :+ Error(SourceRange(pos, pos), msg)
      }

    }

    lex.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)


    val prog = parser.program()

    if (errors.nonEmpty) {
      return ErrorResult(errors)
    }


    val s = prog.toStringTree(parser)

    val inputProg = InputAst.transformProgram(prog)
    NormalResult(inputProg)
  }



  case class SourcePosition(
    line: Int,
    column: Int
  )

  case class SourceRange(
    start: SourcePosition,
    end: SourcePosition
  )

  case class Error(
    position: SourceRange,
    message: String
  )

  sealed abstract class Result[T] {

    def filterWith(p: T => Boolean) = ???

    def map[S](f: T => S): Result[S] = this match {
      case NormalResult(value) => NormalResult(f(value))
      case ErrorResult(errors) => ErrorResult(errors)
    }

    def flatMap[S](f: T => Result[S]): Result[S] = this match {
      case NormalResult(value) => f(value)
      case ErrorResult(errors) => ErrorResult(errors)
    }


    def foreach(fn: T => Unit): Unit = this match {
      case NormalResult(value) => fn(value)
      case ErrorResult(errors) =>
    }
  }

  case class NormalResult[T](
    value: T
  ) extends Result[T]

  case class ErrorResult[T](
    errors: List[Error]
  ) extends Result[T]


}