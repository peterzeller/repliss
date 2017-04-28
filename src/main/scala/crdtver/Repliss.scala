package crdtver

import java.io.{File, FileNotFoundException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import crdtver.InputAst.{InProgram, SourceRange}
import crdtver.WhyAst.Module
import crdtver.parser.{LangLexer, LangParser}
import crdtver.web.ReplissServer
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import scala.collection.immutable.Seq
import scala.util.matching.Regex
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Left, Right, Success}

import scalaz.concurrent.Task
import scalaz._
import scalaz.\/._


object Repliss {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      RunArgs.printHelp()
      return
      //      println("Missing program arguments. Give a filename to check or start the web-server with '-server'.")
      //      System.exit(4)
      //      return
    }
    val runArgs = RunArgs.parse(args.toList).getOrElse {
      return
    }


    if (runArgs.server) {
      ReplissServer.main(args)
      return
    }
//    if (runArgs.quickcheck) {
//      InterpreterTest.main(args)
//      return
//    }

    val inputFile = runArgs.file.getOrElse {
      println("no file given")
      return
    }
    val inputFileStr: String = args(0)
    val input = getInput(inputFileStr)

    try {
      var checks: List[ReplissCheck] = List()
      if (runArgs.quickcheck) {
        checks ::= Quickcheck()
      }

      if (runArgs.verify) {
        checks ::= Verify()
      }


      val res = checkInput(input, inputFileStr, checks)

      res match {
        case NormalResult(result) =>
          val outputLock = new Object

          val counterExampleFut = result.counterexampleFut.map {
            case None =>
              outputLock.synchronized {
                println(" ✓  Random tests ok")
              }
            case Some(counterexample) =>
              outputLock.synchronized {
                println("Found a counter-example:")
                println(s"Assertion in ${counterexample.brokenInvariant} failed!")
                for (i <- counterexample.info) {
                  println(s"   $i")
                }
                println()
                println("Trace:")
                println(counterexample.trace)
                println("")
                println("Calls:")
                for (c <- counterexample.state.calls.values) {
                  println(s"Call ${c.id} in ${c.origin}: ${c.operation}")
                }
              }
              Files.write(Paths.get(s"./model/${Paths.get(inputFileStr).getFileName}.svg"), counterexample.counterExampleSvg.getBytes(StandardCharsets.UTF_8))
              Files.write(Paths.get(s"./model/${Paths.get(inputFileStr).getFileName}.dot"), counterexample.counterExampleDot.getBytes(StandardCharsets.UTF_8))
          }

          for (r <- result.why3ResultStream.iterator) {
            val symbol = r.res match {
              case Valid() => "✓"
              case Timeout() => "⌚"
              case Unknown(s) => s"⁇ ($s)"
              case Why3Error(s) => s"ERROR: $s"

            }

            outputLock.synchronized {
              println(s" $symbol  ${r.proc}")
            }

          }

//          val results = result.why3Results
//          result.counterexample match {
//            case None =>
//              println(" ✓  Random tests ok")
//            case Some(counterexample) =>
//              println("Found a counter-example:")
//              println(s"Assertion in ${counterexample.brokenInvariant} does not hold after executing")
//              println(counterexample.trace)
//              println("")
//          }
//
//          for (r <- results) {
//            val symbol = r.res match {
//              case Valid() => "✓"
//              case Timeout() => "⌚"
//              case Unknown(s) => s"⁇ ($s)"
//              case Why3Error(s) => s"ERROR: $s"
//
//            }
//
//            println(s" $symbol  ${r.proc}")
//          }

          // this blocks until all is done:
          val resValid = result.isValid
          Await.result(counterExampleFut, atMost = 5.seconds)
          println()
          if (resValid) {
            println(" ✓ Program is correct!")
          } else {
            println(" ✗ Verification failed!")
            println(s"( ${result.hasCounterexample} ... ${result.isVerified}")
            System.exit(1)
          }

        case ErrorResult(errors) =>
          val sourceLines = input.lines.toArray

          for (err <- errors) {
            val position = err.position
            val lineNr = position.start.line


            println(s"$inputFileStr line $lineNr: ${err.message}")
            println()
            if (lineNr > 0 && lineNr <= sourceLines.length) {
              val line = sourceLines(lineNr - 1)
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
              println(" " * startCol + "^" * (endCol - startCol))
              println()
            }
          }
          println(" ✗ There are errors in the input program!")
          System.exit(2)
      }
    } catch {
      case (e: FileNotFoundException) =>
        println(e.getMessage)
        System.exit(3)
    }

  }

  def getInput(inputFileStr: String): String = {
    val inputFile = new File(inputFileStr)
    if (inputFile.exists()) {
      return scala.io.Source.fromFile(inputFileStr).mkString
    } else {
      try {
        return Helper.getResource("/examples/" + inputFileStr)
      } catch {
        case (e: FileNotFoundException) =>
          throw new FileNotFoundException(s"Input file $inputFileStr not found.")
      }
    }
  }

  //  @deprecated
  //  def check(inputFileStr: String): Result[List[Why3Result]] = {
  //    val inputFile = new File(inputFileStr)
  //    if (!inputFile.exists()) {
  //      try {
  //        val input = Helper.getResource("/examples/" + inputFileStr)
  //        return checkInput(input, inputFileStr)
  //      } catch {
  //        case (e: FileNotFoundException) =>
  //          throw new FileNotFoundException(s"Input file $inputFileStr not found.")
  //      }
  //    }
  //
  //    for (
  //      inputProg <- parseFile(inputFile);
  //      typedInputProg <- typecheck(inputProg);
  //      whyProg = translateProg(typedInputProg);
  //      why3Result <- checkWhyModule(inputFile.getName, whyProg)
  //    ) yield {
  //      why3Result
  //    }
  //  }

  def parseAndTypecheck(inputName: String, input: String): Result[InProgram] = {
    parseInput(inputName, input).flatMap(typecheck)
  }

  sealed trait ReplissCheck

  case class Verify() extends ReplissCheck

  case class Quickcheck() extends ReplissCheck


  def quickcheckProgram(inputName: String, typedInputProg: InProgram): Option[QuickcheckCounterexample] = {
    val prog = AtomicTransform.transformProg(typedInputProg)

    val interpreter = new Interpreter(prog)
    interpreter.randomTests(limit = 200, threads = 4)
  }


  def checkInput(
    input: String,
    inputName: String,
    checks: List[ReplissCheck] = List(Verify(), Quickcheck())
  ): Result[ReplissResult] = {
    def performChecks(typedInputProg: InProgram): Result[ReplissResult] = {
      //      val why3Task: Future[Result[List[Why3Result]]] = Future {
      //        val whyProg = translateProg(typedInputProg)
      //        checkWhyModule(inputName, whyProg)
      //      }
      //      val quickcheckTask: Future[Option[QuickcheckCounterexample]] = Future {
      //        quickcheckProgram(inputName, typedInputProg)
      //      }


      //      val why3Task = Promise[Result[List[Why3Result]]]
      //      val quickcheckTask = Promise[Option[QuickcheckCounterexample]]
      //      val p = Promise[Either[Result[List[Why3Result]], Option[QuickcheckCounterexample]]]()
      //      p.tryCompleteWith(why3Task.map(Left(_)))
      //      p.tryCompleteWith(quickcheckTask.map(Right(_)))


      val verifyThread = Future {
        if (checks contains Verify()) {
          val whyProg = translateProg(typedInputProg)
          checkWhyModule(inputName, whyProg)
        } else {
          Stream.empty
        }
      }

      val quickcheckThread = Future {
        if (checks contains Quickcheck()) {
          quickcheckProgram(inputName, typedInputProg)
        } else {
          None
        }
      }


      NormalResult(new ReplissResult(
        why3ResultStream = Await.result(verifyThread, Duration.Inf),
        counterexampleFut = quickcheckThread
      ))
    }

    for (
      inputProg <- parseInput(inputName.replace(".rpls", ""), input);
      typedInputProg <- typecheck(inputProg);
      res <- performChecks(typedInputProg)
    ) yield res
  }

  private def checkWhyModule(inputName: String, whyProg: Module): Stream[Why3Result] = {
    val printedWhycode: String = printWhyProg(whyProg)
    //    println(s"OUT = $sb")


    checkWhy3code(inputName, printedWhycode)
  }


  private def checkWhy3code(inputNameRaw: String, printedWhycode: String): Stream[Why3Result] = {
    new File("model").mkdirs()
    val inputName = Paths.get(inputNameRaw).getFileName

    val boogieOutputFile = Paths.get(s"model/$inputName.mlw")
    Files.write(boogieOutputFile, printedWhycode.getBytes(StandardCharsets.UTF_8))

    import sys.process._
    //val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!
    //val why3Result: String = s"why3 prove -P z3 model/$inputName.mlw".!!(logger)

    var why3Result = ""
    var why3Errors = ""

    val resStream = new MutableStream[Why3Result]

    val resultRegexp: Regex = "([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) : ([^ ]+) \\(([0-9.]+)s\\)".r

    def onOutput(line: String): Unit = {
      line match {
        case resultRegexp(file, module, t, proc, resStr, timeStr) =>
          val res = resStr match {
            case "Valid" => Valid()
            case "Timeout" => Timeout()
            case _ => Unknown(resStr)
          }
          val time = timeStr.toDouble
          resStream.push(Why3Result(proc, res, time))
        case _ =>
          println(s"could not parse why3 result $line")
          resStream.push(Why3Result("unknown", Unknown(line), 0))
      }
    }

    def onError(line: String): Unit = {
      resStream.push(Why3Result("unkown", Why3Error(line), 0))
    }

    val why3io = new ProcessIO(
      writeInput = (o: OutputStream) => {},
      processOutput = (is: InputStream) => {
        for (line <- scala.io.Source.fromInputStream(is).getLines()) {
          onOutput(line)
        }
      },
      processError = (is: InputStream) => {
        for (line <- scala.io.Source.fromInputStream(is).getLines()) {
          onError(line)
        }
      },
      daemonizeThreads = false
    )

    // Further why3 options
    // split goals (might be useful for better error messages, why3 --list-transforms for further transforms)
    // -a split_all_full
    // -a simplify_formula
    // -a inline_all  / inline_goal / inline_trivial

    Future {
      val timelimit = 10
      val why3Process = s"why3 prove -P z3 -t $timelimit -a inline_all model/$inputName.mlw".run(why3io)

      val why3exitValue = why3Process.exitValue()
      if (why3exitValue != 0) {


        // we throw an exception here, because this can only happen when there is a bug in code generation
        // all errors in the input should already be caught by type checking
        val message =
        s"""
           |Errors in Why3
           |$why3Errors
           |$why3Result
        """.stripMargin
        resStream.push(Why3Result("unkown", Why3Error(message), 0))
      }
      println("completing why3 stream")
      resStream.complete()
    }

    resStream.stream
  }

  class ReplissResult(
    val why3ResultStream: Stream[Why3Result],
    val counterexampleFut: Future[Option[QuickcheckCounterexample]]
  ) {

    lazy val why3Results: List[Why3Result] = {
      // Await.result(Future.sequence(why3ResultFuts), Duration.Inf)
      why3ResultStream.toList
    }

    lazy val counterexample: Option[QuickcheckCounterexample] = Await.result(counterexampleFut, Duration.Inf)



    // using strict conjunction, so that we wait for both results
    def isValid: Boolean = isVerified & !hasCounterexample

    def isVerified: Boolean = why3Results.forall(r => r.res match {
      case Valid() => true
      case Why3Error(msg) => !msg.toLowerCase.contains("error")
      case _ => false
    })

    def hasCounterexample: Boolean = counterexample.nonEmpty

  }

  case class Why3Result(
    proc: String,
    res: Why3VerificationResult,
    time: Double
  )

  case class QuickcheckCounterexample(
    brokenInvariant: SourceRange,
    info: List[Interpreter.EvalExprInfo],
    trace: String,
    state: Interpreter.State,
    counterExampleSvg: String,
    counterExampleDot: String
  )

  sealed abstract class Why3VerificationResult

  case class Valid() extends Why3VerificationResult

  case class Timeout() extends Why3VerificationResult

  case class Unknown(s: String) extends Why3VerificationResult

  case class Why3Error(s: String) extends Why3VerificationResult

  private def printWhyProg(whyProg: Module)

  = {
    val printer: WhyPrinter = new WhyPrinter()
    val printed = printer.printProgram(whyProg)
    printed
  }

  private def translateProg(typedInputProg: InProgram): Module

  = {
    val translator = new WhyTranslation(
      restrictCalls = Some(1),
      restrictInvocations = Some(1),
      restrictDomains = Some(1)
    )
    val whyProg = translator.transformProgram(typedInputProg)
    whyProg
  }

  private def typecheck(inputProg: InProgram): Result[InProgram]

  = {
    val typer = new Typer()
    typer.checkProgram(inputProg)
    // TODO errorhandling
  }

  private def parseFile(inputFile: File): Result[InProgram]

  = {
    println(s"Reading input $inputFile")

    val input = io.Source.fromFile(inputFile).mkString

    parseInput(inputFile.getName.replace(".rpls",""), input)
  }

  private def parseInput(progName: String, input: String): Result[InProgram]

  = {
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

    val inputProg = InputAst.transformProgram(progName, prog)
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