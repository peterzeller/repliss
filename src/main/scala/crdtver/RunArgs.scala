package crdtver

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
  file: Option[String] = None
) {

}

object RunArgs {

  def parse(args: List[String]): Option[RunArgs] = parser.parse(args, RunArgs())

  def printHelp(): Unit = parser.showUsage()

  private val parser = new scopt.OptionParser[RunArgs]("repliss") {
    head("Repliss")


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

    arg[String]("<file>")
      .optional()
      .action((v, args) => args.copy(file = Some(v)))
      .text("File to parse")

  }
}
