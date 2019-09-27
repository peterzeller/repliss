name := "repliss"

version := "0.1"

scalaVersion := "2.13.1"

mainClass in Compile := Some("crdtver.Repliss")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

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

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"

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

// compiler plugin for handling non-exhaustive matches as errors
//libraryDependencies ++= Seq(
//  compilerPlugin("com.softwaremill.neme" %% "neme-plugin" % "0.0.2")
//)

// Intellij Annotations
libraryDependencies += "org.jetbrains" % "annotations" % "17.0.0"

// Z3 theorem prover:
unmanagedBase := baseDirectory.value / "native" / "bin"
unmanagedResourceDirectories in Compile += baseDirectory.value / "native" / "bin"
//libraryDependencies += "com.microsoft" % "z3" % "4.7.1" from "com.microsoft.z3.jar"

//val libraryDir = file(".") / "z3" / "bin"
//
//javaOptions in run += s"-Djava.library.path=$libraryDir"
envVars := Map("LD_LIBRARY_PATH" -> "./native/bin")

dependencyOverrides += "org.webjars" % "jquery" % "3.1.1-1"

libraryDependencies ++= Seq(
  "org.webjars" % "jquery" % "3.1.1-1",
  "org.webjars" % "bootstrap" % "3.1.1-2",
  "org.webjars" % "ace" % "01.08.2014",
  "org.webjars" % "requirejs" % "2.3.2"
)

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
