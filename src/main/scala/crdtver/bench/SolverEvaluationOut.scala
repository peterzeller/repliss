package crdtver.bench

import java.io.File
import java.time.Duration

import crdtver.Repliss.{checkInput, computeChecks, getInput}
import crdtver.symbolic.smt.Smt.NamedConstraint
import crdtver.symbolic.smt._
import crdtver.utils.DurationUtils.{DurationExt, DurationUnits}
import crdtver.utils.{Helper, TimeTaker}
import crdtver.{Repliss, RunArgs}
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Json, _}

import scala.collection.mutable
import scala.concurrent.Await
import scala.math.Ordered.orderingToOrdered

object SolverEvaluationOut {


  /** compare durations
   * a faster ==> -1 < result < 0
   * b faster ==> 0 < result < 1
   *
   **/
  def compare(a: Duration, b: Duration): Double = {
    val r =
      if (a < b)
        (a / b) - 1
      else if
      (a == b) 0
      else
        -((b / a) - 1)

    r * 100
  }

  case class Example(
       example: String,
       name: String,
       maxPrio: Int
     )

     case class DataPoint(
       solver: String,
       duration: Duration
     )


  def main(args: Array[String]): Unit = {

//    runComp("unsat", "z3", "cvc4")
    runComp("sat", "z3", "cvc4f")

  }

  def runComp(res: String, solver1: String, solver2: String): Unit = {
    val all = SolverEvaluation.allTimes
    val unsatExamples: Map[Example, List[DataPoint]] =
      all.iterator
        .filter(_._2.result == res)
        .map { x =>

          val i = x._1
          val r = x._2
          (Example(i.example, i.name, i.maxPrio), DataPoint(i.solver, r.duration))
        }
        .toList
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap



    var buckets: Map[Int, Int] =
      LazyList.from(-110, 10).takeWhile(_ <= 110).map(x => (x, 0)).toMap


    def insert(ratio: Double): Unit = {
      val bucket =
        buckets.keys.minBy(b => Math.abs(ratio - b))(Ordering.Double.TotalOrdering)

      buckets += bucket -> (buckets(bucket) + 1)
    }

    for ((e, ds) <- unsatExamples) {
      println(e)
      val z3 = ds.find(_.solver == solver1)
      val cvc4 = ds.find(_.solver == solver2)
      (z3, cvc4) match {
        case (Some(z3), Some(cvc4)) =>
          val p = compare(z3.duration, cvc4.duration)
          println(s"$p -- ${z3.duration.formatH} -- ${cvc4.duration.formatH}")
          insert(p)
        case (None, Some(cvc4)) =>
          println(s"$solver2 WIN ${cvc4.duration.formatH}")
          insert(110)
        case (Some(z3), None) =>
          println(s"$solver1 WIN ${z3.duration.formatH}")
          insert(-110)
        case _ =>

      }


    }

    for (k <- buckets.toList.sorted) {
      print(s"(${k._1 - 5}, ${k._2})")
    }
    println()

    println(s"$solver1 faster: ${buckets.filter(_._1 < 0).values.sum}")
    println(s"$solver2 faster: ${buckets.filter(_._1 > 0).values.sum}")

  }


}
