//package crdtver.verification
//
//import java.io.{File, IOException, InputStream, OutputStream}
//import java.nio.charset.StandardCharsets
//import java.nio.file.{Files, Paths}
//import java.util.concurrent.TimeUnit
//
//import crdtver.Repliss._
//import crdtver.utils.MutableStream
//
//import scala.concurrent.duration.Duration
//import scala.sys.process._
//import scala.util.matching.Regex
//
//object Why3Runner {
//
//  private def exitOk(cmd: String): Boolean =
//    try {
//      cmd.run().exitValue() == 0
//    } catch {
//      case e:IOException => false
//    }
//
//
//  private lazy val why3Installed: Boolean = exitOk("why3 --version")
//
//  private lazy val dockerInstalled: Boolean = exitOk("docker --version")
//
//
//  def checkWhy3code(inputNameRaw: String, printedWhycode: String): LazyList[Why3Result] = {
//    new File("model").mkdirs()
//    val inputName = Paths.get(inputNameRaw).getFileName
//
//    val boogieOutputFile = Paths.get(s"model/$inputName.mlw")
//    Files.write(boogieOutputFile, printedWhycode.getBytes(StandardCharsets.UTF_8))
//
//    import sys.process._
//    //val boogieResult: String = "boogie test.bpl /printModel:2 /printModelToFile:model.txt".!!
//    //val why3Result: String = s"why3 prove -P z3 model/$inputName.mlw".!!(logger)
//
//    val why3Result = ""
//    val why3Errors = ""
//
//    val resStream = new MutableStream[Why3Result]
//
//    val resultRegexp: Regex = "([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) : ([^ ]+) \\(([0-9.]+)s\\)".r
//
//    def onOutput(line: String): Unit = {
//      line match {
//        case resultRegexp(file, module, t, proc, resStr, timeStr) =>
//          val res = resStr match {
//            case "Valid" => Valid()
//            case "Timeout" => Timeout()
//            case _ => Unknown(resStr)
//          }
//          val time = timeStr.toDouble
//          resStream.push(Why3Result(proc, res, Duration(time, TimeUnit.SECONDS)))
//        case _ =>
//          println(s"could not parse why3 result $line")
//          resStream.push(Why3Result("unknown", Unknown(line), Duration.Zero))
//      }
//    }
//
//    def onError(line: String): Unit = {
//      resStream.push(Why3Result("unkown", Why3Error(line), Duration.Zero))
//    }
//
//    val why3io = new ProcessIO(
//      writeInput = (o: OutputStream) => {},
//      processOutput = (is: InputStream) => {
//        for (line <- scala.io.Source.fromInputStream(is).getLines()) {
//          onOutput(line)
//        }
//      },
//      processError = (is: InputStream) => {
//        for (line <- scala.io.Source.fromInputStream(is).getLines()) {
//          onError(line)
//        }
//      },
//      daemonizeThreads = false
//    )
//
//    val timelimit = 10
//    val why3Options = List("prove", "-P", "z3", "-t", timelimit.toString)
//    val why3Command: ProcessBuilder =
//      if (why3Installed) {
//        Process("why3", why3Options ++ List(s"model/$inputName.mlw"))
//      } else if (dockerInstalled) {
//        val modelDir = new File("./model")
//        Process("docker",
//          List("run",
//            "--rm",
//            "--mount", s"type=bind,source=${modelDir.getAbsoluteFile},target=/why3,readonly",
//            "peterzel/why3:0.87.3",
//            "why3")
//            ++ why3Options
//            ++ List(s"/why3/$inputName.mlw"))
//      } else {
//        throw new RuntimeException("Either why3 or docker must be installed and available on the PATH for repliss to run.")
//      }
//
//    // Further why3 options
//    // split goals (might be useful for better error messages, why3 --list-transforms for further transforms)
//    // -a split_all_full
//    // -a simplify_formula
//    // -a inline_all  / inline_goal / inline_trivial
//
//    val thread = new Thread {
//      override def run(): Unit = {
//        try {
//
//          // interesting options: -a inline_all
//          val why3Process = why3Command.run(why3io)
//
//          val why3exitValue = why3Process.exitValue()
//          if (why3exitValue != 0) {
//
//
//            // we throw an exception here, because this can only happen when there is a bug in code generation
//            // all errors in the input should already be caught by type checking
//            val message = "Errors in Why3:\n" + why3Errors + "\n\n" + why3Result
//            resStream.
//              push(Why3Result(
//                "unkown",
//                Why3Error(message), Duration.Zero))
//          }
//        } catch {
//          case e: Throwable =>
//            resStream.
//              push(Why3Result(
//                "unkown",
//                Why3Error("Error while executing why3: " + e), Duration.Zero))
//        } finally {
//          resStream.complete()
//        }
//      }
//    }
//    thread.start()
//
//    resStream.stream
//  }
//
//
//}
