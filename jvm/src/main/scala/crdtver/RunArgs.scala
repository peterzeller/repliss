package crdtver

import java.util.concurrent.TimeUnit

import crdtver.symbolic.smt.Solver

import scala.concurrent.duration.Duration

case class RunArgs(
  server: Boolean = false,
  quickcheck: Boolean = false,
  quickcheckDomainSize: Int = 3,
  quickcheckMaxUsedIds: Int = 2,
  threads: Int = 4,
  stepsPerRun: Int = 100,
  numberOfRuns: Int = 60,
  smallCheck: Boolean = false,
  smallCheck2: Boolean = false,
  symbolicCheck: Boolean = false,
  verify: Boolean = false,
  sessionIds: Boolean = true,
  inferShapeInvariants: Boolean = true,
  host: String = "localhost",
  port: Int = 8080,
  file: Option[String] = None,
  printVersion: Boolean = false,
  timeout: Duration = Duration(30, TimeUnit.MINUTES),
  solver: Solver = Solver.parseSolver(
    //    "0.2cvc4"
    //    "0.2(cvc4|z3);cvc4f"
    //    "(Icvc4)|(Iz3)|(Icvc4f)"
    "I(cvc4|cvc4f)"
  )
) {

}

object RunArgs {

  def parse(args: List[String]): Option[RunArgs] = parser.parse(args, RunArgs())

  def printHelp(): Unit = parser.showUsage()

  private val parser = new scopt.OptionParser[RunArgs]("repliss") {
    head("Repliss")

    arg[String]("<file>")
      .optional()
      .action((v, args) => args.copy(file = Some(v)))
      .text("File to parse")

    opt[Unit]("version")
      .action((v, args) => args.copy(printVersion = true))
      .text("Print version information")

    opt[Unit]("server")
      .action((v, args) => args.copy(server = true))
      .text("Starts a server")

    opt[String]('h', "host")
      .action((v, args) => args.copy(host = v))
      .text("Specifies on which host the server should run")

    opt[Int]('p', "port")
      .action((v, args) => args.copy(port = v))
      .text("Specifies on which port the server should run")


    opt[Unit]("quickcheck")
      .action((v, args) => args.copy(quickcheck = true))
      .text("Runs random tests on the input program")

    opt[Unit]("smallcheck")
      .action((v, args) => args.copy(smallCheck = true))
      .text("Runs automatic tests on the input program exploring all small executions")

    opt[Unit]("smallcheck2")
      .action((v, args) => args.copy(smallCheck2 = true))
      .text("Runs automatic tests on the input program exploring all small executions (with duplicate state detection)")

    opt[Unit]("session-ids")
      .action((v, args) => args.copy(sessionIds = true))
      .text("Identifier-types are used with session guarantees, i.e. when a procedure is called with an identifier it is ensured that the generating invocation happened before.")

    opt[Unit]("symbolicCheck")
      .action((v, args) => args.copy(symbolicCheck = true))
      .text("Verify the program using symbolic execution.")

    opt[Unit]("noShapeInvariants")
      .action((v, args) => args.copy(inferShapeInvariants = false))
      .text("Do not automatically infer shape invariants.")

    opt[Duration]("timeout")
      .action((v, args) => args.copy(timeout = v))
      .text("Maximum time for each individual checks. Use a format like --timeout 30s or --timeout 2min (see scala.concurrent.duration.Duration)")

    opt[String]("solver")
      .action((v, args) => args.copy(solver = Solver.parseSolver(v)))
      .text(
        """The theorem solver to use for verification.
          |
          |        Supported solvers:
          |
          |         s: = cvc4
          |            | cvc4f
          |            | z3
          |            | s|s        // parallel solve
          |            | s;s        // sequential solve
          |            | I s        // incrementally adding facts
          |            | float s    // scaled timeout
          |            | (s)
          |
          |         Examples: "z3", "cvc4", "I(cvc4|cvc4f)"
          |""".stripMargin)

    opt[Int]("quickcheckDomainSize")
      .action((v, args) => args.copy(quickcheckDomainSize = v))
      .text("Domain size for custom types in quickcheck tests.")

    opt[Int]("quickcheckMaxUsedIds")
      .action((v, args) => args.copy(quickcheckMaxUsedIds = v))
      .text("Maximum number of identifiers generated for id types in quickcheck tests.")

    opt[Int]("threads")
      .validate(v => if (v < 0) Left("Must have at least one thread") else Right(()))
      .action((v, args) => args.copy(threads = v))
      .text("Number of threads to use for testing.")

    opt[Int]("numberOfRuns")
      .action((v, args) => args.copy(numberOfRuns = v))
      .text("How many executions to execute via quickcheck.")

    opt[Int]("stepsPerRun")
      .action((v, args) => args.copy(stepsPerRun = v))
      .text("Number of steps for each quickcheck run.")

  }
}
