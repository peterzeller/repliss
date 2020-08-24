package crdtver

import java.io.{File, FileNotFoundException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.{Duration => _}
import java.{time, util}

import crdtver.language.ExtendedLexer.ExtendedReplissLexer
import crdtver.language.InputAst.{SourcePosition, SourceRange}
import crdtver.language.TypedAst.{InProgram, SourceRange}
import crdtver.language._
import crdtver.parser.{LangLexer, LangParser}
import crdtver.symbolic.{ShapeAnalysis, SymbolicEvaluator, SymbolicExecutionRes}
import crdtver.testing.Visualization.RenderResult
import crdtver.testing.{Interpreter, RandomTester, SmallcheckTester}
import crdtver.utils.DurationUtils._
import crdtver.utils.LazyListUtils.LazyListExtensions
import crdtver.utils.{ConcurrencyUtils, Helper, MutableStream, ReplissVersion}
import crdtver.web.ReplissServer
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import scala.collection.immutable.StringOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Using
import scala.util.matching.Regex


object Repliss {

  def exitCode(res: Result[ReplissResult]): Int = res match {
    case NormalResult(value) =>
      if (value.isValid) 0 else 1
    case ErrorResult(errors) =>
      2
  }

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

    if (runArgs.printVersion) {
      val v = ReplissVersion.version
      print(
        s"""
           |Version ${v.version}
           |Git ${v.git} (${v.date})
           |""".stripMargin)
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

      if (runArgs.smallCheck) {
        checks ::= SmallCheck()
      }

      if (runArgs.verify) {
        checks ::= Verify()
      }

      if (runArgs.symbolicCheck) {
        checks ::= SymbolicCheck()
      }


      val res = checkInput(input, inputFile, checks, runArgs)

      printError(runArgs, inputFile, input, checks, res)
      System.exit(exitCode(res))
    } catch {
      case e: FileNotFoundException =>
        println(e.getMessage)
        System.exit(3)
    }

  }

  private def printError(runArgs: RunArgs, inputFile: String, input: String, checks: List[ReplissCheck], res: Result[ReplissResult]): Unit = {
    res match {
      case NormalResult(result) =>
        val outputLock = new Object

        val counterExampleFut: Future[Unit] =
          if (runArgs.quickcheck) {
            printTestingResultQuickCheck(result, inputFile, outputLock)
          } else {
            Future(())
          }

        val counterExampleSmallCheckFut: Future[Unit] =
          if (runArgs.smallCheck) {
            printTestingResultSmallCheck(result, inputFile, outputLock)
          } else {
            Future(())
          }

        val symbolicExecutionResultsFut: Future[Unit] =
          if (runArgs.symbolicCheck)
            Future(printSymbolicExecutionResult(result, inputFile, outputLock))
          else
            Future(())

        outputWhy3Results(result, outputLock)


        // this blocks until all is done:
        val resValid = result.isValid
        Await.result(
          Future.sequence(List(
            counterExampleFut,
            counterExampleSmallCheckFut,
            symbolicExecutionResultsFut)),
          atMost = 5.seconds)
        println()
        if (resValid) {
          println(s" ✓ All ${checks.length} checks passed!")
        } else {
          println(" ✗ Correctness checks failed!")
          val results = List(
            "found counter example in QuickCheck testing" -> result.hasCounterexample,
            "found counter example in SmallCheck testing" -> result.hasSmallCheckCounterexample,
            "found counter example in symbolic execution" -> result.hasSymbolicCounterexample,
            "failed verfication" -> !result.isVerified
          )

          println(s"(${results.filter(_._2).map(_._1).mkString(" and ")})")
        }

      case ErrorResult(errors) =>
        val sourceLines: Array[String] = new StringOps(input).linesIterator.toArray
        for (err <- errors) {
          val position = err.position
          val lineNr = position.start.line


          println(s"Error in $inputFile:")
          println()
          val sampleLines = sourceLines.zipWithIndex.slice(lineNr - 3, lineNr)
          if (sampleLines.nonEmpty) {
            val line = sampleLines.last._1
            val startCol = position.start.column
            var endCol =
              if (position.stop.line == position.start.line)
                position.stop.column
              else
                line.length
            if (endCol <= startCol) {
              endCol = startCol + 1
            }
            for ((l, nr) <- sampleLines) {
              val lineNrStr = String.format("%4d", nr + 1) + " | "
              print(lineNrStr)
              println(l.replace('\t', ' '))
            }
            println(" " * (7 + startCol) + "^" * (endCol - startCol))
            println(err.message)
            println(" \n")
          }
        }
        println(" ✗ There are errors in the input program!")
    }
  }

  def printSymbolicExecutionResult(result: ReplissResult, inputFile: String, outputLock: Object): Unit = {
    val startTime = System.currentTimeMillis()
    var lastTime = startTime

    def takeTime(): time.Duration = {
      val t = System.currentTimeMillis()
      val dur = t - lastTime
      lastTime = t
      time.Duration.ofMillis(dur)
    }

    for (r <- result.symbolicExecutionResultStream.iterator) {
      r.error match {
        case None =>
          val t = takeTime()
          println(s" ✓ ${r.proc} (${t.formatH})")
        case Some(counterexample) =>
          outputLock.synchronized {
            val t = takeTime()
            println("❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌")
            println(s" ERROR: ${r.proc}")
            println(s"Found a problem in line ${counterexample.errorLocation.start.line}:")
            println(s" ${counterexample.message}")
            println()
            println(counterexample.trace)
            println("\n\n\n")
            val inputFileName = Paths.get(inputFile).getFileName

            val modelFolder = Paths.get("model", inputFileName.getFileName.toString)
            if (!modelFolder.toFile.exists() && !modelFolder.toFile.mkdirs()) {
              throw new RuntimeException(s"could not create dir ${modelFolder.toAbsolutePath}")
            }

            val isaFile = modelFolder.resolve(s"${r.proc}.thy")
            Files.write(isaFile, counterexample.translation.isabelleTranslation.getBytes(StandardCharsets.UTF_8))
            println(s"Written Isabelle export to ${isaFile.toUri}")

            val cvcFile = modelFolder.resolve(s"${r.proc}.cvc")
            Files.write(cvcFile, counterexample.translation.cvcTranslation.getBytes(StandardCharsets.UTF_8))
            println(s"Written CVC export to ${cvcFile.toUri}")
            println()

            val smtFile = modelFolder.resolve(s"${r.proc}.smt")
            Files.write(smtFile, counterexample.translation.smtTranslation.getBytes(StandardCharsets.UTF_8))
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
                      Files.write(svgPath, ce.renderResult.svg.getBytes(StandardCharsets.UTF_8))
                      val dotPath = modelFolder.resolve(s"${r.proc}_$i.dot")
                      Files.write(dotPath, ce.renderResult.dot.getBytes(StandardCharsets.UTF_8))
                      val modelPath = modelFolder.resolve(s"${r.proc}$i.txt")
                      Files.write(modelPath, ce.modelText.prettyStr(120).getBytes(StandardCharsets.UTF_8))

                      println(s"  Written model to         ${modelPath.toUri}")
                      println(s"  Written visualization to ${svgPath.toUri}")
                    case None =>
                  }
                }
                println(s"Time: ${t.formatH}")
                println("\n")
            }
            for (exc <- r.exception) {
              exc.printStackTrace(System.out)
            }
          }
      }
    }
    val dur = time.Duration.ofMillis(System.currentTimeMillis() - startTime)
    println(s"Overall symbolic execution time: ${dur.formatH}")
  }

  def printTestingResultQuickCheck(result: ReplissResult, inputFile: String, outputLock: Object): Future[Unit] = {
    printTestingResult("QuickCheck", result.counterexampleFut, inputFile, outputLock)
  }

  private def printTestingResult(name: String, fut: Future[Option[QuickcheckCounterexample]], inputFile: String, outputLock: Object): Future[Unit] = {
    fut.map {
      case None =>
        outputLock.synchronized {
          println(s" ✓  $name tests ok")
        }
      case Some(counterexample) =>
        printCounterexample(s"while running $name", inputFile, outputLock, counterexample)
    }
  }

  def printTestingResultSmallCheck(result: ReplissResult, inputFile: String, outputLock: Object): Future[Unit] = {
    printTestingResult("SmallCheck", result.counterexampleSmallCheckFut, inputFile, outputLock)
  }

  private def printCounterexample(where: String, inputFile: String, outputLock: Object, counterexample: QuickcheckCounterexample) = {
    outputLock.synchronized {
      println(s"Found a counter-example $where:")
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
    Files.write(Paths.get(s"./model/${Paths.get(inputFile).getFileName}.svg"), counterexample.counterExampleRender.svg.getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get(s"./model/${Paths.get(inputFile).getFileName}.dot"), counterexample.counterExampleRender.dot.getBytes(StandardCharsets.UTF_8))
  }

  private def outputWhy3Results(result: ReplissResult, outputLock: Object) = {
    for (r <- result.why3ResultStream.iterator) {
      val symbol = r.res match {
        case Valid() => "✓"
        case Timeout() => "⌚"
        case Unknown(s) => s"⁇ ($s)"
        case Why3Error(s) =>
          s"""
             |❌❌❌❌❌❌❌❌❌❌❌❌❌❌❌
             |ERROR: $s
             |
             |
             |
             |""".stripMargin

      }

      outputLock.synchronized {
        println(s" $symbol  ${r.proc}")
      }

    }
  }

  def getInput(inputFileStr: String): String = {
    val inputFile = new File(inputFileStr)
    if (inputFile.exists()) {
      Using(scala.io.Source.fromFile(inputFileStr))(_.mkString).get
    } else {
      try {
        Helper.getResource("/examples/" + inputFileStr)
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

  def parseAndTypecheck(inputName: String, input: String, inferShapeInvariants: Boolean = true): Result[InProgram] = {
    parseInput(inputName, input)
      .flatMap(typecheck)
      .map(AtomicTransform.transformProg)
      .map(TypeMonomorphization.monomorphizeProgram)
      .map({ prog =>
        if (inferShapeInvariants)
          new ShapeAnalysis().inferInvariants(prog)
        else prog
      })
  }

  sealed trait ReplissCheck

  case class Verify() extends ReplissCheck

  case class Quickcheck() extends ReplissCheck

  case class SmallCheck() extends ReplissCheck

  case class SymbolicCheck() extends ReplissCheck


  def quickcheckProgram(inputName: String, prog: TypedAst.InProgram, runArgs: RunArgs): Option[QuickcheckCounterexample] = {
    val tester = new RandomTester(prog, runArgs)
    tester.randomTests(limit = 100, threads = 60, timeLimit = runArgs.timeout)
  }


  def smallCheckProgram(inputName: String, prog: TypedAst.InProgram, runArgs: RunArgs): Option[QuickcheckCounterexample] = {
    ConcurrencyUtils.withTimeoutOpt(
      timeout = runArgs.timeout,
      name = "SmallCheck-tests",
      work = {
        val tester = new SmallcheckTester(prog, runArgs)
        tester.randomTestsSingle(limit = 5000)
      }
    )
  }

  def symbolicCheckProgram(inputName: String, prog: TypedAst.InProgram, runArgs: RunArgs): LazyList[SymbolicExecutionRes] = {
    val tester = new SymbolicEvaluator(prog, runArgs)
    tester.checkProgram()
  }


  def checkInput(
    input: String,
    inputName: String,
    checks: List[ReplissCheck],
    runArgs: RunArgs
  ): Result[ReplissResult] = {
    def performChecks(typedInputProg: TypedAst.InProgram): Result[ReplissResult] = {

      val verifyThread: Future[LazyList[Why3Result]] = Future {
        if (checks contains Verify()) {
          throw new RuntimeException("no longer supported")
        } else {
          LazyList.empty
        }
      }

      val quickcheckThread: Future[Option[QuickcheckCounterexample]] = Future {
        if (checks contains Quickcheck()) {
          quickcheckProgram(inputName, typedInputProg, runArgs)
        } else {
          None
        }
      }

      val smallCheckThread: Future[Option[QuickcheckCounterexample]] = Future {
        if (checks contains SmallCheck()) {
          smallCheckProgram(inputName, typedInputProg, runArgs)
        } else {
          None
        }
      }

      val symbolicCheckThread: Future[LazyList[SymbolicExecutionRes]] = Future {
        if (checks contains SymbolicCheck()) {
          symbolicCheckProgram(inputName, typedInputProg, runArgs)
            // only take checks until first procedure failing with exception
            .takeUntil(_.exception.nonEmpty)
        } else {
          LazyList()
        }
      }


      NormalResult(new ReplissResult(
        typedProgram = typedInputProg,
        why3ResultStream = Await.result(verifyThread, Duration.Inf),
        counterexampleFut = quickcheckThread,
        counterexampleSmallCheckFut = smallCheckThread,
        symbolicExecutionResultStream = Await.result(symbolicCheckThread, Duration.Inf)
      ))
    }

    val inputName2 = inputName.replace(".rpls", "")
    //    println(s"#### input ####\n${input}")
    for {
      typedInputProg <- parseAndTypecheck(inputName2, input, runArgs.inferShapeInvariants)
      //      _ = println(s"#### typed ####\n${typedInputProg.printAst}")
      //      _ = println(s"#### typed mono ####\n${mProg.printAst}")
      res <- performChecks(typedInputProg)
    } yield res
  }


  private def checkWhy3code(inputNameRaw: String, printedWhycode: String): LazyList[Why3Result] = {
    new File("model").mkdirs()
    val inputName = Paths.get(inputNameRaw).getFileName

    val boogieOutputFile = Paths.get(s"model/$inputName.mlw")
    Files.write(boogieOutputFile, printedWhycode.getBytes(StandardCharsets.UTF_8))

    import sys.process._
    //val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!
    //val why3Result: String = s"why3 prove -P z3 model/$inputName.mlw".!!(logger)

    val why3Result = ""
    val why3Errors = ""

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
          resStream.push(Why3Result(proc, res, Duration(time, SECONDS)))
        case _ =>
          println(s"could not parse why3 result $line")
          resStream.push(Why3Result("unknown", Unknown(line), 0.seconds))
      }
    }

    def onError(line: String): Unit = {
      resStream.push(Why3Result("unkown", Why3Error(line), 0.seconds))
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
      // interesting options: -a inline_all
      val why3Process = s"why3 prove -P z3 -t $timelimit model/$inputName.mlw".run(why3io)

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
        resStream.push(Why3Result("unkown", Why3Error(message), 0.seconds))
      }
      resStream.complete()
    }

    resStream.stream
  }

  class ReplissResult(
    val typedProgram: TypedAst.InProgram,
    val why3ResultStream: LazyList[Why3Result],
    val counterexampleFut: Future[Option[QuickcheckCounterexample]],
    val counterexampleSmallCheckFut: Future[Option[QuickcheckCounterexample]],
    val symbolicExecutionResultStream: LazyList[SymbolicExecutionRes]
  ) {

    lazy val why3Results: List[Why3Result] = {
      // Await.result(Future.sequence(why3ResultFuts), Duration.Inf)
      why3ResultStream.toList
    }

    lazy val counterexample: Option[QuickcheckCounterexample] = Await.result(counterexampleFut, Duration.Inf)
    lazy val smallCheckCounterexample: Option[QuickcheckCounterexample] = Await.result(counterexampleSmallCheckFut, Duration.Inf)

    lazy val symbolicCounterexample: List[SymbolicExecutionRes] = symbolicExecutionResultStream.toList


    // using strict conjunction, so that we wait for both results
    def isValid: Boolean = isVerified & !hasCounterexample & !hasSmallCheckCounterexample & !hasSymbolicCounterexample

    def isVerified: Boolean = why3Results.forall(r => r.res match {
      case Valid() => true
      case Why3Error(msg) => !msg.toLowerCase.contains("error")
      case _ => false
    })

    def hasCounterexample: Boolean = counterexample.nonEmpty

    def hasSmallCheckCounterexample: Boolean = smallCheckCounterexample.nonEmpty

    def hasSymbolicCounterexample: Boolean = symbolicCounterexample.exists(r => {
      r.exception.foreach(exc => throw exc)
      r.error.isDefined
    })

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
    counterExampleRender: RenderResult
  )

  sealed abstract class Why3VerificationResult

  case class Valid() extends Why3VerificationResult

  case class Timeout() extends Why3VerificationResult

  case class Unknown(s: String) extends Why3VerificationResult

  case class Why3Error(s: String) extends Why3VerificationResult


  private def typecheck(inputProg: InputAst.InProgram): Result[InProgram]

  = {
    val typer = new Typer()
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
    val lex = new ExtendedReplissLexer(inStream)
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
    parser.removeErrorListeners()
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
