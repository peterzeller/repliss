package crdtver

import java.io.{File, FileNotFoundException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util

import crdtver.language.InputAst.{SourcePosition, SourceRange}
import crdtver.language.TypedAst.{InProgram, SourceRange}
import crdtver.language._
import crdtver.language.crdts.CrdtContext
import crdtver.parser.{LangLexer, LangParser}
import crdtver.symbolic.{SymbolicEvaluator, SymbolicExecutionException, SymbolicExecutionRes}
import crdtver.testing.{Interpreter, RandomTester}
import crdtver.utils.{Helper, MutableStream}
import crdtver.web.ReplissServer
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import scala.collection.immutable.StringOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex


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
    println(s"Checking file $inputFile ...")
    val input = getInput(inputFile)

    try {
      var checks: List[ReplissCheck] = List()
      if (runArgs.quickcheck) {
        checks ::= Quickcheck()
      }

      if (runArgs.verify) {
        checks ::= Verify()
      }

      if (runArgs.symbolicCheck) {
        checks ::= SymbolicCheck()
      }


      val res = checkInput(input, inputFile, checks, runArgs)

      res match {
        case NormalResult(result) =>
          val outputLock = new Object

          val counterExampleFut: Future[Unit] =
            if (runArgs.quickcheck) {
              printTestingResult(result, inputFile, outputLock)
            } else {
              Future(())
            }

          printSymbolicExecutionResult(result, inputFile, outputLock)


          // this blocks until all is done:
          val resValid = result.isValid
          Await.result(counterExampleFut, atMost = 5.seconds)
          println()
          if (resValid) {
            println(s" ✓ All ${checks.length} checks passed!")
          } else {
            println(" ✗ Verification failed!")
            val results = List(
              "found counter example in random testing" -> result.hasCounterexample,
              "found counter example in symbolic execution" -> result.hasSymbolicCounterexample
            )

            println(s"(${results.filter(_._2).map(_._1).mkString(" and ")})")
            System.exit(1)
          }

        case ErrorResult(errors) =>
          val sourceLines: Array[String] = new StringOps(input).linesIterator.toArray
          for (err <- errors) {
            val position = err.position
            val lineNr = position.start.line


            println(s"$inputFile line $lineNr: ${err.message}")
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

  def printSymbolicExecutionResult(result: ReplissResult, inputFile: String, outputLock: Object): Unit = {
    for (r <- result.symbolicExecutionResultStream.iterator) {
      r.error match {
        case None =>
          println(s" ✓ ${r.proc}")
        case Some(counterexample) =>
          outputLock.synchronized {
            println(s" ERROR: ${r.proc}")
            println(s"Found a problem in line ${counterexample.errorLocation.start.line}:")
            println(s" ${counterexample.message}")
            println()
            println(counterexample.trace)
            println("")
            val inputFileName = Paths.get(inputFile).getFileName

            val modelFolder = Paths.get("model", inputFileName.getFileName.toString)
            if (!modelFolder.toFile.exists() && !modelFolder.toFile.mkdirs()) {
              throw new RuntimeException(s"could not create dir ${modelFolder.toAbsolutePath}")
            }

            val isaFile = modelFolder.resolve(s"${r.proc}.thy")
            Files.write(isaFile, counterexample.isabelleTranslation.getBytes(StandardCharsets.UTF_8))
            println(s"Written Isabelle export to ${isaFile.toUri}")

            val smtFile =  modelFolder.resolve(s"${r.proc}.cvc")
            Files.write(smtFile, counterexample.smtTranslation.getBytes(StandardCharsets.UTF_8))
            println(s"Written SMT export to ${smtFile.toUri}")
            println()

            counterexample.trace.lastStep.flatMap(_.info) match {
              case None =>
                println(s" Could not compute model")
              case Some(ce) =>
                println("Calls:")
                for (c <- ce.state.calls.values) {
                  println(s"Call ${c.id} in ${c.callTransaction} in ${c.origin}: ${c.operation}")
                }
                println("\n")

                for ((step, i) <- counterexample.trace.steps.zipWithIndex) {
                  println(step.description)
                  step.info match {
                    case Some(ce) =>
                      val svgPath = modelFolder.resolve(s"${r.proc}_$i.svg")
                      Files.write(svgPath, ce.counterExampleSvg.getBytes(StandardCharsets.UTF_8))
                      val dotPath = modelFolder.resolve(s"${r.proc}_$i.dot")
                      Files.write(dotPath, ce.counterExampleDot.getBytes(StandardCharsets.UTF_8))
                      val modelPath = modelFolder.resolve(s"${r.proc}$i.txt")
                      Files.write(modelPath, ce.modelText.prettyStr(120).getBytes(StandardCharsets.UTF_8))

                      println(s"  Written model to         ${modelPath.toUri}")
                      println(s"  Written visualization to ${svgPath.toUri}")
                    case None =>
                  }
                }
                println("\n")
            }
          }
      }
    }
  }

  def printTestingResult(result: ReplissResult, inputFile: String, outputLock: Object): Future[Unit] = {
    result.counterexampleFut.map {
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
        Files.write(Paths.get(s"./model/${Paths.get(inputFile).getFileName}.svg"), counterexample.counterExampleSvg.getBytes(StandardCharsets.UTF_8))
        Files.write(Paths.get(s"./model/${Paths.get(inputFile).getFileName}.dot"), counterexample.counterExampleDot.getBytes(StandardCharsets.UTF_8))
    }
  }


  def getInput(inputFileStr: String): String = {
    val inputFile = new File(inputFileStr)
    if (inputFile.exists()) {
      scala.io.Source.fromFile(inputFileStr).mkString
    } else {
      try {
        Helper.getResource("/examples/" + inputFileStr)
      } catch {
        case e: FileNotFoundException =>
          throw new FileNotFoundException(s"Input file $inputFileStr not found.\n$e")
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

  def parseAndTypecheck(inputName: String, input: String)(implicit nameContext: CrdtContext): Result[InProgram] = {
    parseInput(inputName, input).flatMap(typecheck)
  }

  sealed trait ReplissCheck

  case class Verify() extends ReplissCheck

  case class Quickcheck() extends ReplissCheck

  case class SymbolicCheck() extends ReplissCheck


  def quickcheckProgram(inputName: String, typedInputProg: TypedAst.InProgram, runArgs: RunArgs)(implicit nameContext: CrdtContext): Option[QuickcheckCounterexample] = {
    val prog = AtomicTransform.transformProg(typedInputProg)

    val tester = new RandomTester(prog, runArgs)
    tester.randomTests(limit = 200, threads = 8)
  }

  def symbolicCheckProgram(inputName: String, typedInputProg: TypedAst.InProgram, runArgs: RunArgs)(implicit nameContext: CrdtContext): LazyList[SymbolicExecutionRes] = {
    val prog = AtomicTransform.transformProg(typedInputProg)

    val tester = new SymbolicEvaluator(prog)
    tester.checkProgram()
  }


  def checkInput(
    input: String,
    inputName: String,
    checks: List[ReplissCheck],
    runArgs: RunArgs
  ): Result[ReplissResult] = {
    implicit val nameContext: CrdtContext = new CrdtContext()

    def performChecks(typedInputProg: TypedAst.InProgram): Result[ReplissResult] = {
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



      val quickcheckThread: Future[Option[QuickcheckCounterexample]] = Future {
        if (checks contains Quickcheck()) {
          quickcheckProgram(inputName, typedInputProg, runArgs)
        } else {
          None
        }
      }

      val symbolicCheckThread: Future[LazyList[SymbolicExecutionRes]] = Future {
        if (checks contains SymbolicCheck()) {
          symbolicCheckProgram(inputName, typedInputProg, runArgs)
        } else {
          LazyList()
        }
      }


      NormalResult(new ReplissResult(
        counterexampleFut = quickcheckThread,
        symbolicExecutionResultStream = Await.result(symbolicCheckThread, Duration.Inf)
      ))
    }



    for (
      inputProg <- parseInput(inputName.replace(".rpls", ""), input);
      typedInputProg <- typecheck(inputProg);
      res <- performChecks(typedInputProg)
    ) yield res
  }


  class ReplissResult(
    val counterexampleFut: Future[Option[QuickcheckCounterexample]],
    val symbolicExecutionResultStream: LazyList[SymbolicExecutionRes]
  ) {


    lazy val counterexample: Option[QuickcheckCounterexample] = Await.result(counterexampleFut, Duration.Inf)

    lazy val symbolicCounterexample: List[SymbolicExecutionRes] = symbolicExecutionResultStream.toList


    // using strict conjunction, so that we wait for both results
    def isValid: Boolean = !hasCounterexample & !hasSymbolicCounterexample


    def hasCounterexample: Boolean = counterexample.nonEmpty

    def hasSymbolicCounterexample: Boolean = symbolicCounterexample.exists(r => r.error.isDefined)

  }

  case class Why3Result(
    proc: String,
    res: Why3VerificationResult,
    time: Duration
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


  private def typecheck(inputProg: InputAst.InProgram)(implicit nameContext: CrdtContext): Result[InProgram]

  = {
    val typer = new Typer(nameContext)
    typer.checkProgram(inputProg)
    // TODO errorhandling
  }

  private def parseFile(inputFile: File): Result[InputAst.InProgram]

  = {
    println(s"Reading input $inputFile")

    val input = scala.io.Source.fromFile(inputFile).mkString

    parseInput(inputFile.getName.replace(".rpls", ""), input)
  }

  def parseInput(progName: String, input: String): Result[InputAst.InProgram] = {
    val inStream = CharStreams.fromString(input)
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

    val inputProg = AntlrAstTransformation.transformProgram(progName, prog)
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
    position: TypedAst.SourceRange,
    message: String
  )

  sealed abstract class Result[T] {


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

    def hasErrors: Boolean

    def get(): T
  }

  case class NormalResult[T](
    value: T
  ) extends Result[T] {
    def hasErrors = false

    def get(): T = value
  }

  case class ErrorResult[T](
    errors: List[Error]
  ) extends Result[T] {
    def hasErrors = true

    def get() = throw new RuntimeException(s"Errors: $errors")
  }


}