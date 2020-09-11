package crdtver

import java.io.File

import crdtver.Repliss.{checkInput, computeChecks, getInput}
import crdtver.utils.DurationUtils.DurationExt
import crdtver.utils.{Helper, TimeTaker}
import java.time.Duration

import io.circe.Json
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object Benchmark {

  val resultFile: File = new File("model/bench.json")

  var results: Map[List[String], Option[Duration]] = Map()

  def runBenchmark(args: List[String]): Unit = {
    val name = args.mkString(" ")
    val runArgs = RunArgs.parse(args).get


    val inputFile = runArgs.file.get
    val input = getInput(inputFile)

    val checks: scala.List[_root_.crdtver.Repliss.ReplissCheck] = computeChecks(runArgs)

    val (dur, foundBug) = TimeTaker.measure { () =>
      println(s"Running $name")
      val r = checkInput(input, inputFile, checks, runArgs).get()
      !r.isValid
    }
    println(s"foundBug = $foundBug, dur = ${dur.formatH}")
    results += args -> (if (foundBug) Some(dur) else None)

  }


  def main(args: Array[String]): Unit = {

    val examples: List[String] = List(
      "buggy/chatapp_fail1.rpls",
      "buggy/chatapp_fail2.rpls",
      "buggy/userbase_fail1.rpls",
      "buggy/userbase_fail2.rpls",
    )

    val options: List[List[String]] = List(
      List("--quickcheck"),
      List("--smallcheck"),
      List("--smallcheck2", "--timeout", "30min"),
    )

    for (o <- options; e <- examples) {
      runBenchmark(e :: o)
    }

    println("New results: ")
    for ((n, t) <- results.toList.sortBy(_._1.toString())) {
      println(s"$n: $t")
    }

    val oldTimes: Map[List[String], Option[Duration]] =
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

    val mergedTimes = oldTimes ++ results

    val json = mergedTimes.toList.sortBy(_._1.toString()).asJson

    Helper.writeFile(resultFile, json.pretty(Printer.indented("  ")))

    println(s"Json: \n$json")

    println("All results: ")

    for ((n, t) <- mergedTimes.toList.sortBy(_._1.toString())) {
      println(s"${n.mkString(" ")}: ${t.map(_.formatH).getOrElse("-")}")
    }



  }

}
