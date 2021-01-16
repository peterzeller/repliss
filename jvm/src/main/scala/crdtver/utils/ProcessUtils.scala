package crdtver.utils

import java.io.InputStream


object ProcessUtils {

  import sys.process._

  def convertStreamToString(is: InputStream): String =
    scala.io.Source.fromInputStream(is).mkString

  case class RunCommandOutput(
    stdout: String,
    stderr: String
  )

  def runCommand(cmd: Seq[String], in: String): RunCommandOutput = {
    var res = ""
    var err = ""
    val io = new ProcessIO(
      stdin => {
        stdin.write(in.getBytes)
        stdin.close()
      },
      stdout => {
        res = convertStreamToString(stdout)
        stdout.close()
      },
      stderr => {
        err = convertStreamToString(stderr)
        stderr.close()
      })
    val proc = Process(cmd).run(io)
    val exitValue = proc.exitValue()
    if (exitValue != 0) {
      throw new RuntimeException(s"Process exited with code $exitValue\n$res\n$err")
    }
    RunCommandOutput(res, err)
  }
}
