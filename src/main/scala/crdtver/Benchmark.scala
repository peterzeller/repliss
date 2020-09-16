package crdtver

import java.io.{File, PrintStream}

import crdtver.Repliss.{checkInput, computeChecks, getInput}
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.{Helper, TimeTaker}
import java.time.Duration

import crdtver.symbolic.smt.{Cvc4Solver, Z3Solver}
import crdtver.utils.LoggingPrintStream.capturePrintStream
import io.circe.Json
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import scala.concurrent.{Await, duration}

object Benchmark {

  val resultFile: File = new File("model/bench.json")

  var results: Map[List[String], Option[Duration]] = Map()
  var allTimes: Map[List[String], Option[Duration]] = Map()

  case class Err(args: List[String], message: String)

  var failed: List[Err] = List()

  def runBenchmark(args: List[String], expected: Boolean): Unit = {

    val name = args.mkString(" ")
    val runArgs = RunArgs.parse(args).get


    val inputFile = runArgs.file.get
    val input = getInput(inputFile)

    val checks: scala.List[_root_.crdtver.Repliss.ReplissCheck] = computeChecks(runArgs)

    val (dur, ok) = TimeTaker.measure { () =>
      println(s"Running $name")
      val r = checkInput(input, inputFile, checks, runArgs).get()
      val f = Repliss.printResults(r, runArgs, System.out)
      Await.result(f, duration.Duration.Inf)
      val isOk = r.isValid == expected
      if (!isOk) {
        val err = capturePrintStream { out =>
          Repliss.printResults(r, runArgs, out)
        }
        failed = failed :+ Err(args, err)
      }
      isOk
    }
    println(s"ok = $ok, dur = ${dur.formatH}")
    val entry = args -> (if (ok) Some(dur) else None)
    results += entry
    allTimes += entry
    writeAllTimes()

  }


  def runBenchmarks(bs: List[(List[String], Boolean)]): Unit = {
    for ((args, r) <- bs) {
      try {
        runBenchmark(args, r)
      } catch {
        case t: Throwable =>
          failed = failed :+ Err(args, s"Failed with exception ${t.getClass} $t\n${Helper.printStacktrace(t)}")
      }
    }
  }


  def combine(examples: List[String], options: List[List[String]], expectValid: Boolean): List[(List[String], Boolean)] = {
    for (e <- examples; o <- options) yield {
      val args = e :: o
      RunArgs.parse(args).get
      (args, expectValid)
    }
  }


  def main(args: Array[String]): Unit = {

    println(s"z3 version: ${Z3Solver.version()}")
    println(s"cvc4 version: ${Cvc4Solver.version()}")

    val incremental = !args.contains("--clean")

    val buggyExamples: List[String] = List(
      "buggy/chatapp_fail1.rpls",
      "buggy/chatapp_fail2.rpls",
      "buggy/userbase_fail1.rpls",
      "buggy/userbase_fail2.rpls",
    )

    val verifiedExamples: List[String] = List(
      "verified/chatapp.rpls",
      "verified/chatapp_data.rpls",
      "verified/userbase.rpls",
    )

    // verified examples that require shape invariants:
    val verifiedExamplesSi: List[String] = verifiedExamples ++ List(
      "verified/chatapp_si.rpls",
      "verified/userbase2.rpls",
      "verified/userbase3.rpls",
    )

    val buggyOptions: List[List[String]] = List(
      List("--quickcheck"),
      List("--smallcheck"),
      List("--smallcheck2"),
    )

//    val verifiedOptions: List[List[String]] = List(
//      List("--symbolicCheck", "--solver", "cvc4"),
//      List("--symbolicCheck", "--solver", "z3"),
//      List("--symbolicCheck", "--solver", "cvc4f"),
//      List("--symbolicCheck", "--solver", "Icvc4"),
//      List("--symbolicCheck", "--solver", "Iz3"),
//      List("--symbolicCheck", "--solver", "Icvc4f"),
//      List("--symbolicCheck", "--solver", "cvc4|z3|cvc4f"),
//      List("--symbolicCheck", "--solver", "I(cvc4|z3|cvc4f)"),
//    ).map("--noShapeInvariants" :: _)
//
//    val verifiedOptionsSi = List(
//      List("--symbolicCheck", "--solver", "Icvc4"),
//      List("--symbolicCheck", "--solver", "Iz3"),
//      List("--symbolicCheck", "--solver", "Icvc4f"),
//      List("--symbolicCheck", "--solver", "I(cvc4|z3|cvc4f)"),
//    ).map("--noShapeInvariants" :: _)

    val verifiedOptions: List[List[String]] = List(
      List("--symbolicCheck", "--noShapeInvariants")
    )

    val verifiedOptionsSi = List(
      List("--symbolicCheck")
    )

    allTimes =
      if (resultFile.exists()) {
        val contents = Helper.readFile(resultFile)
        decode[List[(List[String], Option[Duration])]](contents) match {
          case Left(e) =>
            throw e
          case Right(v) => v.toMap
        }
      } else {
        Map()
      }

    val allTests: List[(List[String], Boolean)] =
      combine(buggyExamples, buggyOptions, false) ++
        combine(verifiedExamples, verifiedOptions, true) ++
        combine(verifiedExamplesSi, verifiedOptionsSi, true)


    runBenchmarks(
      allTests.filter{t =>
        !incremental || !allTimes.contains(t._1)
      }
    )

    println("New results: ")
    for ((n, t) <- results.toList.sortBy(_._1.toString())) {
      println(s"$n: $t")
    }


    val json = writeAllTimes()
    println(s"Json: \n$json")

    for (f <- failed) {
      print(s"\n\nFAILED ${f.args.mkString(" ")}")
      println(f.message)
    }

    println("All results: ")

    for ((n, t) <- allTimes.toList.sortBy(_._1.toString())) {
      println(s"${n.mkString(" ")}: ${t.map(_.formatH).getOrElse("-")}")
    }




  }

  private def writeAllTimes(): Json = {
    val json = allTimes.toList.sortBy(_._1.toString()).asJson

    Helper.writeFile(resultFile, json.pretty(Printer.indented("  ")))
    json
  }
}
