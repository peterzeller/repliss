name := "repliss"

version := "0.1"

scalaVersion := "2.11.8"

mainClass in Compile := Some("crdtver.Repliss")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

// for debugging:
//javaOptions in reStart += "-agentlib:jdwp=transport=dt_socket,server=y,address=5005,suspend=n"
fork in run := true
cancelable in Global := true

//libraryDependencies += "org.scalameta" %% "scalameta" % "1.0.0"


enablePlugins(Antlr4Plugin)
antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.7.1"
antlr4PackageName in Antlr4 := Some("crdtver.parser")
antlr4GenVisitor in Antlr4 := true
//javaSource in Antlr4 := (baseDirectory / "src-gen").value
//
//unmanagedSourceDirectories in Compile += baseDirectory.value / "src-gen"


libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

val http4sVersion = "0.15.2"

// Only necessary for SNAPSHOT releases
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5")

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion
)

libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.1"

libraryDependencies += "org.http4s" %% "http4s-json4s-native" % "0.15.2"

// scopt for parsing commandline args
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"


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
