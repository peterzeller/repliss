package crdtver

import java.io.{File, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import crdtver.InputAst.{AstElem, InProgram}
import crdtver.Typer.TypeErrorException
import crdtver.WhyAst.Module
import crdtver.parser.{LangLexer, LangParser}
import org.antlr.v4.runtime.atn.{ATNConfigSet, ATNSimulator}
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
    val res = check(inputFileStr)

    res match {
      case NormalResult(results) =>
        for (r <- results) {
          val symbol = r.res match {
            case Valid() => "✓"
            case Timeout() => "⌚"
            case Unknown(s) => s"⁇ ($s)"
          }

          println(s" $symbol  ${r.proc}")
        }
        println()
        val success = results.forall(r => r.res == Valid())
        if (success) {
          println(" ✓ Program is correct!")
        } else {
          println(" ✗ Verification failed!")
          System.exit(1)
        }

      case ErrorResult(errors) =>
        val sourceLines = scala.io.Source.fromFile(inputFileStr).getLines().toArray

        for (err <- errors) {
          val position = err.position
          val lineNr = position.start.line


          println(s"$inputFileStr line $lineNr: ${err.message}")
          println()
          if (lineNr > 0 && lineNr <= sourceLines.length) {
            val line = sourceLines(lineNr-1)
            val startCol = position.start.column
            var endCol =
              if (position.stop.line == position.start.line)
                position.stop.column
              else
                line.length
            if (endCol <= startCol) {
              endCol = startCol + 1
            }

            println(line.replace('\t', ' '))
            println(" "*startCol + "^"*(endCol-startCol))
            println()
          }
        }
        println(" ✗ There are errors in the input program!")
        System.exit(2)
    }

  }


  def check(inputFileStr: String): Result[List[Why3Result]] = {
    val inputFile = new File(inputFileStr)
    if (!inputFile.exists()) {
      throw new RuntimeException(s"Input file $inputFileStr not found.")
    }

    for (
      inputProg <- parseFile(inputFile);
      typedInputProg <- typecheck(inputProg);
      whyProg = translateProg(typedInputProg);
      why3Result <- checkWhyModule(inputFile.getName, whyProg)
    ) yield {
      why3Result
    }
  }

  private def checkWhyModule(inputName: String, whyProg: Module): Result[List[Why3Result]] = {
    val printedWhycode: String = printWhyProg(whyProg)
    //    println(s"OUT = $sb")


    val why3Result = checkWhy3code(inputName, printedWhycode)
    NormalResult(why3Result)
  }


  private def checkWhy3code(inputName: String, printedWhycode: String): List[Why3Result] = {
    new File("model").mkdirs()

    val boogieOutputFile = Paths.get(s"model/$inputName.mlw")
    Files.write(boogieOutputFile, printedWhycode.getBytes(StandardCharsets.UTF_8))

    println("Starting why3")

    import sys.process._
    //val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!
    //val why3Result: String = s"why3 prove -P z3 model/$inputName.mlw".!!(logger)

    var why3Result = ""
    var why3Errors = ""

    val why3io = new ProcessIO(
      writeInput = (o: OutputStream) => {},
      processOutput = (is:  InputStream) => {
        why3Result = scala.io.Source.fromInputStream(is).mkString
      },
      processError = (is:  InputStream) => {
        why3Errors = scala.io.Source.fromInputStream(is).mkString
      },
      daemonizeThreads = false
    )
    val why3Process = s"why3 prove -P z3 model/$inputName.mlw".run(why3io)

    val why3exitValue = why3Process.exitValue()
    if (why3exitValue != 0) {
      // we throw an exception here, because this can only happen when there is a bug in code generation
      // all errors in the input should already be caught by type checking
      throw new RuntimeException(
        s"""
           |Errors in Why3
           |$why3Errors
           |$why3Result
         """.stripMargin)
    }


    val resultRegexp: Regex = "([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) : ([^ ]+) \\(([0-9.]+)s\\)".r

    for (line <- why3Result.split("\n").toList) yield {
      line match {
        case resultRegexp(file, module, t, proc, resStr, timeStr) =>
          val res = resStr match {
            case "Valid" => Valid()
            case "Timeout" => Timeout()
            case _ => Unknown(resStr)
          }
          val time = timeStr.toDouble
          Why3Result(proc, res, time)
        case _ =>
          println(s"could not parse why3 result $line")
          Why3Result("unknown", Unknown(line), 0)
      }
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
    typer.checkProgram(inputProg)
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
        val pos = InputAst.SourcePosition(line, charPositionInLine)
        errors = errors :+ Error(InputAst.SourceRange(pos, pos), msg)
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



//  case class SourcePosition(
//    line: Int,
//    column: Int
//  )
//
//  case class SourceRange(
//    start: SourcePosition,
//    end: SourcePosition
//  )
//

  case class Error(
    position: InputAst.SourceRange,
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

    def hasErrors(): Boolean

    def get(): T
  }

  case class NormalResult[T](
    value: T
  ) extends Result[T] {
    def hasErrors() = false

    def get() = value
  }

  case class ErrorResult[T](
    errors: List[Error]
  ) extends Result[T] {
    def hasErrors() = true

    def get() = throw new RuntimeException(s"Errors: $errors")
  }


}