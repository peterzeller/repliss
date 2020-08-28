package crdtver

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

case class RunArgs(
  server: Boolean = false,
  quickcheck: Boolean = false,
  smallCheck: Boolean = false,
  symbolicCheck: Boolean = false,
  verify: Boolean = false,
  sessionIds: Boolean = true,
  inferShapeInvariants: Boolean = true,
  host: String = "localhost",
  port: Int = 8080,
  file: Option[String] = None,
  printVersion: Boolean = false,
  timeout: Duration = Duration(5, TimeUnit.MINUTES),
  solverCvc4: Boolean = true,
  solverZ3: Boolean = true
) {

}

object RunArgs {

  def parse(args: List[String]): Option[RunArgs] = parser.parse(args, RunArgs())

  def printHelp(): Unit = parser.showUsage()

  private val parser = new scopt.OptionParser[RunArgs]("repliss") {
    head("Repliss")

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

    arg[String]("<file>")
      .optional()
      .action((v, args) => args.copy(file = Some(v)))
      .text("File to parse")

    opt[Unit]("no-z3")
      .action((v, args) => args.copy(solverZ3 = false))
      .text("Disables the Z3 theorem solver")

    opt[Unit]("no-cvc4")
      .action((v, args) => args.copy(solverCvc4 = false))
      .text("Disables the Z3 theorem solver")

  }
}
