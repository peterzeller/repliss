package crdtver.bench

import java.io.File
import java.time.Duration

import crdtver.Repliss.{checkInput, computeChecks, getInput}
import crdtver.RunArgs
import crdtver.bench.Benchmark.{allTimes, resultFile}
import crdtver.symbolic.smt.{Cvc4Solver, SmtBuildModel, SmtBuildUnsatCore, SmtTimeout, Solver, Z3Solver}
import crdtver.utils.DurationUtils.DurationUnits
import crdtver.utils.{DurationUtils, Helper, MathUtils, TimeTaker}
import io.circe.{Json, _}
import io.circe.parser._
import io.circe.syntax._
import java.io.File
import java.lang.Double.parseDouble
import java.lang.Runtime.getRuntime
import java.time.Duration

import crdtver.Repliss.{checkInput, computeChecks, getInput}
import crdtver.bench.RecordingSolver.Check
import crdtver.symbolic.smt.Smt.NamedConstraint
import crdtver.symbolic.smt.{Cvc4Solver, Z3Solver}
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.LoggingPrintStream.capturePrintStream
import crdtver.utils.MathUtils.{max, min}
import crdtver.{Repliss, RunArgs}
import io.circe.{Json, _}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, duration}
import scala.math.Ordered.orderingToOrdered
import crdtver.symbolic.ShapeAnalysis

object SolverEvaluation {


  def runExample(example: String): Unit = {
    RecordingSolver.currentInput = example

    val runArgs = RunArgs(
      file = Some(example),
      solver = RecordingSolver,
      symbolicCheck = true
    )

    val inputFile = runArgs.file.get
    val input = getInput(inputFile)
    val checks: scala.List[_root_.crdtver.Repliss.ReplissCheck] = computeChecks(runArgs)
    println(s"running $example")
    val r = checkInput(input, inputFile, checks, runArgs).get()
    val f = Repliss.printResults(r, runArgs, System.out)
    Await.result(f, scala.concurrent.duration.Duration.Inf)
    // wait for result
    r.isValid


  }

  val resultFile: File = new File("model/bench_solver_eval.json")

  case class Input(
    example: String,
    name: String,
    maxPrio: Int,
    solver: String,
    timeout: Duration
  )

  case class Result(
    duration: Duration,
    result: String
  )


  var allTimes: Map[Input, Result] =
    if (resultFile.exists()) {
      val contents = Helper.readFile(resultFile)
      decode[List[(Input, Result)]](contents) match {
        case Left(e) =>
          throw e
        case Right(v) => v.toMap
      }
    } else {
      Map()
    }

  private def writeAllTimes(): Json = {
    val json = allTimes.toList.sortBy(_._1.toString()).asJson

    Helper.writeFile(resultFile, json.pretty(Printer.indented("  ")))
    json
  }


  val solvers = List(
    Solver.parseSolver("I(cvc4|cvc4f)"),
    new Cvc4Solver(finiteModelFind = true),
    new Cvc4Solver(finiteModelFind = false),
    new Z3Solver(),
  )

  /** The minimum time required to solve */
  def minTime(input: Input): Duration = {
    allTimes.filter { k =>
      val i = k._1
      i.name == input.name &&
        i.example == input.example &&
        i.maxPrio == input.maxPrio &&
        k._2.result != "unknown"
    }.map(_._2.duration)
      .minOption
      .getOrElse(30.minutes)
  }

  /** Do we know wether this is sat or unsat? */
  def isSat(input: Input): Option[Boolean] = {
    val rs = for (s <- solvers) yield {
      allTimes.get(input.copy(solver = s.toString))
    }
    rs.flatten.map(_.result).filter(_ != "unknown").map(_ == "sat").headOption
  }

  /** which solvers know that this input is unsatisfiable? */
  def isKnownUnsatBy(input: Input): Map[String, Duration] = {
    allTimes.iterator
      .filter { k =>
      val i = k._1
      i.name == input.name &&
        i.example == input.example &&
        i.solver == input.solver &&
        i.maxPrio <= input.maxPrio &&
        k._2.result == "unsat"
    }.map(x => x._1.solver -> x._2.duration)
      .toList
      .groupMapReduce(_._1)(_._2)(min(_, _))
  }


  def main(args: Array[String]): Unit = {

    val examples = List(
      "buggy/chatapp_fail1.rpls",
      "buggy/chatapp_fail2.rpls",
      "buggy/userbase_fail1.rpls",
      "buggy/userbase_fail2.rpls",
      "failsToVerify/chatapp1.rpls",
      "failsToVerify/chatapp2.rpls",
      "failsToVerify/chatapp3.rpls",
      "failsToVerify/chatapp_si1.rpls",
      "failsToVerify/chatapp_si2.rpls",
      "failsToVerify/userbase_si.rpls",
      "verified/chatapp.rpls",
      "verified/chatapp_data.rpls",
      "verified/chatapp_si.rpls",
      "verified/userbase.rpls",
      "verified/userbase2.rpls",
      "verified/userbase3.rpls",
    )

    for (example <- examples) {
      runExample(example)
    }


    case class Check2(
      input: Input,
      constraints: List[NamedConstraint],
      solver: Solver
    ) {
      def updatedTimeout(f: Duration => Duration): Check2 =
        copy(input = input.copy(timeout = f(input.timeout)))

    }

    val checks = RecordingSolver.getChecks
    val inputs: List[Check2] = checks.flatMap { check =>
      val prios: List[Int] = check.constraints.map(_.priority).filter(x => !check.isTiny || x < 150).sorted.distinct
      (for (prio <- prios) yield {
        val constraints = check.constraints.filter(_.priority <= prio)

        for (s <- solvers) yield {
          Check2(
            Input(
              check.input,
              check.name,
              prio,
              s.toString,
              30.minutes
            ), constraints, s)
        }
      }).flatten
    }

    val q = new mutable.Queue[Check2]
    q.enqueueAll(inputs)

    val processPid = ProcessHandle.current.pid()

    while (q.nonEmpty) {


      val check = q.dequeue()
      val input = check.input

      val minT = minTime(input)

      val options = List(SmtTimeout(input.timeout), SmtBuildModel(), SmtBuildUnsatCore())

      val knownUnsatSolvers = isKnownUnsatBy(input)

      println(s"Running $input")
      if (knownUnsatSolvers.contains(input.solver))
        println(s"\tAlready solved in ${knownUnsatSolvers(input.solver)}")
      else if (isSat(input).contains(true))
        println("\tIs SAT")
      else if (input.timeout > max(1.seconds, minT * 20))
        println(s"\tAlready solved by other solver in ${minT.formatH}")
      else {


        val currentEntry = allTimes.get(input)
        currentEntry match {
          case None =>
            checkMemory(processPid)

            val s = check.solver
            val constraints = check.constraints
            println(s"${q.size}\talready solved by $knownUnsatSolvers")
            val (dur, res) = TimeTaker.measure { () => s.check(constraints, options, s"${input.example}_${input.name}_${input.maxPrio}_${s.toString}") }
            val r = Result(
              dur,
              res match {
                case _: Solver.Satisfiable => "sat"
                case _: Solver.Unknown =>
                  q.enqueue(check.updatedTimeout(_ * 2))
                  "unknown"
                case _: Solver.Unsatisfiable => "unsat"
              }
            )
            allTimes += input -> r
            println(s"  Result = ${r.result} \t // ${r.duration.formatH}")
            writeAllTimes()
          case Some(e) =>
            println(s"\tAlready done $e")
            if (e.result == "unknown")
              q.enqueue(check.updatedTimeout(_ * 2))
        }
      }
    }

  }


  private def checkMemory(processPid: Long): Unit = {
    import scala.sys.process._
    val memoryStr = s"ps -o %mem $processPid".!!
    val memory = parseDouble(memoryStr.filter(c => c.isDigit || c == '.'))

    println(s"mem: $memory")
    if (memory > 75) {
      System.exit(0)
    }
  }
}
