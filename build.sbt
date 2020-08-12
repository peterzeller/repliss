import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

name := "repliss"

version := "0.1"

scalaVersion := "2.13.1"

mainClass in Compile := Some("crdtver.Repliss")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

// for debugging:
//javaOptions in reStart += "-agentlib:jdwp=transport=dt_socket,server=y,address=5005,suspend=n"
fork in run := true
cancelable in Global := true

//libraryDependencies += "org.scalameta" %% "scalameta" % "1.0.0"

// String similarity for error messages
libraryDependencies += "info.debatty" % "java-string-similarity" % "1.2.1"


enablePlugins(Antlr4Plugin)
antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.7.1"
antlr4PackageName in Antlr4 := Some("crdtver.parser")
antlr4GenVisitor in Antlr4 := true
javaSource in Antlr4 := (sourceManaged in Compile).value
//javaSource in Antlr4 := (baseDirectory / "src-gen").value
//
//unmanagedSourceDirectories in Compile += baseDirectory.value / "src-gen"


//libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

libraryDependencies += "codes.reactive" %% "scala-time" % "0.4.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

libraryDependencies += "junit" % "junit" % "4.12" % Test

val http4sVersion = "0.21.0-M1"

// Only necessary for SNAPSHOT releases
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5")

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.12.0-M3",
  // Optional for string interpolation to JSON model
  "io.circe" %% "circe-literal" % "0.12.0-M3"
)

libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.7.0"

libraryDependencies += "org.http4s" %% "http4s-json4s-native" % http4sVersion

// shapeless for generic programming
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

// scopt for parsing commandline args
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"

// Flexmark for showing Markdown documentation
libraryDependencies += "com.vladsch.flexmark" % "flexmark-all" % "0.35.10"

// compiler plugin for handling non-exhaustive matches as errors
//libraryDependencies ++= Seq(
//  compilerPlugin("com.softwaremill.neme" %% "neme-plugin" % "0.0.2")
//)

// Intellij Annotations
libraryDependencies += "org.jetbrains" % "annotations" % "17.0.0"

// For scala test: Generating Html reports
libraryDependencies += "com.vladsch.flexmark" % "flexmark-all" % "0.35.10" % Test
testOptions in Test ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")
)
// Ordered output on console:
logBuffered in Test := false
parallelExecution in Test := false
testOptions in Test ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-oDFU")
)

// add documentation to classpath:
unmanagedResourceDirectories in Compile += baseDirectory.value / "documentation"


dependencyOverrides += "org.webjars" % "jquery" % "3.1.1-1"

libraryDependencies ++= Seq(
  "org.webjars" % "jquery" % "3.1.1-1",
  "org.webjars" % "bootstrap" % "3.1.1-2",
  "org.webjars" % "ace" % "01.08.2014",
  "org.webjars" % "requirejs" % "2.3.2"
)

// Task for downloading CVC4 binaries:
lazy val downloadCvc4 = taskKey[Unit]("Download CVC4 binaries")

downloadCvc4 := {
  val downloads = List(
    "libcvc4parser.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/53143b717d50309807389c5c787ac675/libcvc4parser.so",
    "libcvc4.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/784548991ba8e74de75abcb06f60cfa4/libcvc4.so",
//    "libcvc4.so.6" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/37a3400d3a487fcc48e6c0bbda0a73f5/libcvc4.so.6",
    "libcvc4jni.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/ae711a63ae127992f59b8a7f8ba33ce5/libcvc4jni.so",
    "libz3java.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/eec6e02d8d93a4d1a533f569f461ab26/libz3java.so",
    "libz3.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/1b4f6161ca6a68aa3a731f28b0eb1c3d/libz3.so"
  )


  val path = Paths.get("src", "main", "resources", "native")
  val pathF = path.toFile
  if (!pathF.exists() && !pathF.mkdirs()) {
    throw new Exception(s"Could not create folder '$path'")
  }

  for ((f,d) <- downloads) {
    val file = path.resolve(f).toFile
    if (!file.exists()) {

      import sys.process._
      (new URL(d) #> file).!!
    }
  }

}

(compile in Compile) := ((compile in Compile) dependsOn downloadCvc4).value

// Task for adding version information
lazy val versionFile = taskKey[Unit]("Generate version information file")

versionFile := {
  import sys.process._

  val versionFile = Paths.get("src/main/resources/versioninfo.properties")

  val gitRevision = ("git describe --tags --always".!!).trim
  val gitLastCommitTime = "git log -1 --format=%cd".!!
  val formatIn = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy ZZZZ")
  val date = formatIn.parse(gitLastCommitTime)
  val formatOut = new java.text.SimpleDateFormat("yyyy-MM-dd")

  val fileContents =
    s"""
      |version=${(version in ThisBuild).value}
      |gitRevision=$gitRevision
      |gitDate=${formatOut.format(date)}
      |""".stripMargin

  val currentContents = if (versionFile.toFile.exists()) Files.readString(versionFile, StandardCharsets.UTF_8) else ""

  if (fileContents != currentContents) {
    Files.write(versionFile, fileContents.getBytes(StandardCharsets.UTF_8))
  }

}

(compile in Compile) := ((compile in Compile) dependsOn versionFile).value

// Task for building js dependencies:
lazy val buildJs = taskKey[Long]("Build Javascript files")

buildJs := {
  import sbt.util.CacheImplicits._

  def maxChangedTime(f: File): Long = {
    val files = f.listFiles() match {
      case null => List()
      case ar => ar.toList
    }
    val subTimes = for (sub <- files) yield maxChangedTime(sub)
    (f.lastModified() :: subTimes.toList).max
  }

  def deleteFiles(f: File): Unit = {
    if (f.isDirectory) {
      for (sub <- f.listFiles())
        deleteFiles(sub)
    } else {
      Files.deleteIfExists(f.toPath)
    }
  }

  def copyFiles(from: File, to: File): Unit = {
    if (from.isDirectory) {
      to.mkdirs()
      for (sub <- from.listFiles()) {
        copyFiles(sub, to / sub.name)
      }
    } else {
      Files.copy(from.toPath, to.toPath)
    }
  }

  val jsProjDir = new File("./js")
  val lastModDate: Long = List(
    maxChangedTime(jsProjDir / "src"),
    maxChangedTime(jsProjDir / "public"),
    maxChangedTime(jsProjDir / "build.sbt")
  ).max
  val oldModDate: Long = buildJs.previous.getOrElse(0)
  if (lastModDate != oldModDate) {
    println(s"updating JS ($lastModDate != $oldModDate)")
    val pb = new ProcessBuilder("sbt", "fastOptJS::webpack")
    pb.directory(jsProjDir)
    pb.inheritIO()
    val p = pb.start()
    p.waitFor() match {
      case 0 => // ok
      case n => throw new Exception(s"build failed: $n")
    }
    val target = new File(".") / "src" / "main" / "resources" / "scalajs"
    val source = jsProjDir / "target" / "scala-2.12" / "scalajs-bundler" / "main" / "dist"
    deleteFiles(target)
    copyFiles(source, target)
  }
  lastModDate
}

(compile in Compile) := ((compile in Compile) dependsOn buildJs).value


// Task for building js dependencies:
lazy val docker = taskKey[Unit]("Build Docker image")

docker := {
  assembly.value
  import sys.process._
  "docker build -t peterzel/repliss .".!
}



// Gnieh Pretty Printer (https://github.com/gnieh/gnieh-pp)
//libraryDependencies += "org.gnieh" % "gnieh-pp_2.10" % "0.1"
// see project/Build.scala (no 2.11 version currently on maven)

// do not run tests tagged as slow by default
testOptions in Test += Tests.Argument("-l", "org.scalatest.tags.Slow")

// add a special config to run the slow tests:
// sbt slow:test
lazy val Slow = config("slow").extend(Test)
configs(Slow)
inConfig(Slow)(Defaults.testTasks)
testOptions in Slow -= Tests.Argument("-l", "org.scalatest.tags.Slow")
testOptions in Slow += Tests.Argument("-n", "org.scalatest.tags.Slow")

scalacOptions += "-Yrangepos"
