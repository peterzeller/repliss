import org.scalajs.linker.interface.ModuleKind.CommonJSModule

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Locale

val http4sVersion = "0.21.13"


lazy val repliss =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("."))
    .settings(
      organization := "com.github.peterzeller",
      scalaVersion := "2.13.4",
      //scalacOptions ++= Seq("-encoding", "UTF-8", "-feature", "-unchecked", "-Xlint", "-deprecation"),
      //testFrameworks += new TestFramework("utest.runner.Framework"),
      /* shared dependencies */
      libraryDependencies ++= Seq(
      )
    )


// Task for downloading CVC4 binaries:
lazy val downloadCvc4 = taskKey[Unit]("Download CVC4 binaries")

// Task for adding version information
lazy val versionFile = taskKey[Unit]("Generate version information file")

// Tag for slow tests
//lazy val Slow = config("slow").extend(Test)
//configs(Slow)
//inConfig(Slow)(Defaults.testTasks)
//testOptions in Test += Tests.Argument("-l", "org.scalatest.tags.Slow")
//testOptions in Slow -= Tests.Argument("-l", "org.scalatest.tags.Slow")
//testOptions in Slow += Tests.Argument("-n", "org.scalatest.tags.Slow")

lazy val replissJvm: Project =
  repliss.jvm
    .enablePlugins(WebScalaJSBundlerPlugin)
    .enablePlugins(Antlr4Plugin)
    .settings(
      name := "replissJVM",
      mainClass  in (Compile, run) := Some("crdtver.Repliss"),
      mainClass in assembly := Some("crdtver.Repliss"),
      resolvers += Resolver.sonatypeRepo("snapshots"),
      resolvers += "jitpack" at "https://jitpack.io",
      /* Normal scala dependencies */
      libraryDependencies ++= Seq(
        "info.debatty" % "java-string-similarity" % "1.2.1",
        "org.apache.commons" % "commons-lang3" % "3.4",
        "codes.reactive" %% "scala-time" % "0.4.2",
        "org.scala-lang" % "scala-reflect" % "2.13.0",
        "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
        "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
        "org.typelevel" %% "cats-core" % "2.0.0",
        "com.github.peterzeller" % "logic-evaluator" % "439d251b5b",
        "org.slf4j" % "slf4j-api" % "1.7.5",
        "org.slf4j" % "slf4j-simple" % "1.7.5",
        "org.http4s" %% "http4s-dsl" % http4sVersion,
        "org.http4s" %% "http4s-blaze-server" % http4sVersion,
        "org.http4s" %% "http4s-blaze-client" % http4sVersion,
        "org.http4s" %% "http4s-circe" % http4sVersion,
        "org.http4s" %% "http4s-json4s-native" % http4sVersion,
        // Optional for auto-derivation of JSON codecs
        "io.circe" %% "circe-generic" % "0.12.0-M3",
        // Optional for string interpolation to JSON model
        "io.circe" %% "circe-literal" % "0.12.0-M3",
        "io.circe" %% "circe-parser" % "0.12.0-M3",
        "com.lihaoyi" %% "scalatags" % "0.7.0",
        // shapeless for generic programming
        "com.chuusai" %% "shapeless" % "2.3.3",
        // scopt for parsing commandline args
        "com.github.scopt" %% "scopt" % "3.7.1",
        // Flexmark for showing Markdown documentation
        "com.vladsch.flexmark" % "flexmark-all" % "0.35.10",
        // Intellij Annotations
        "org.jetbrains" % "annotations" % "17.0.0",
        //  parser combinators
        "com.lihaoyi" %% "fastparse" % "2.3.0",
        // web jars:
        "org.webjars" % "jquery" % "3.1.1-1",
        "org.webjars" % "bootstrap" % "3.1.1-2",
        "org.webjars" % "ace" % "01.08.2014",
        "org.webjars" % "requirejs" % "2.3.2",
        // For scala test: Generating Html reports
        "com.vladsch.flexmark" % "flexmark-all" % "0.35.10" % Test,
        // Testing libraries:
        "org.scalatest" %% "scalatest" % "3.1.0" % Test,
        "junit" % "junit" % "4.12" % Test,
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
      ),
      dependencyOverrides += "org.webjars" % "jquery" % "3.1.1-1",
      scalaJSProjects := Seq(replissJs),
      Assets / pipelineStages := Seq(scalaJSPipeline),
      antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.7.1",
      antlr4PackageName in Antlr4 := Some("crdtver.parser"),
      antlr4GenVisitor in Antlr4 := true,
      javaSource in Antlr4 := (sourceManaged in Compile).value,
      testOptions in Test ++= Seq(
        Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")
      ),
      // Ordered output on console:
      logBuffered in Test := false,
      parallelExecution in Test := false,
      testOptions in Test ++= Seq(
        Tests.Argument(TestFrameworks.ScalaTest, "-oDFU")
      ),

      // add documentation to classpath:
      unmanagedResourceDirectories in Compile += baseDirectory.value / "documentation",
      // Do not run tests for assembly task
      test in assembly := {},
      downloadCvc4 := {
        val downloads = List(
          "libcvc4parser.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/53143b717d50309807389c5c787ac675/libcvc4parser.so",
          "libcvc4.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/784548991ba8e74de75abcb06f60cfa4/libcvc4.so",
          //    "libcvc4.so.6" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/37a3400d3a487fcc48e6c0bbda0a73f5/libcvc4.so.6",
          "libcvc4jni.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/ae711a63ae127992f59b8a7f8ba33ce5/libcvc4jni.so",
          "libz3java.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/eec6e02d8d93a4d1a533f569f461ab26/libz3java.so",
          "libz3.so" -> "https://softech-git.informatik.uni-kl.de/zeller/repliss/uploads/1b4f6161ca6a68aa3a731f28b0eb1c3d/libz3.so"
        )


        val path = Paths.get("jvm", "src", "main", "resources", "native")
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

      },
      (compile in Compile) := ((compile in Compile) dependsOn downloadCvc4).value,
      versionFile := {
        import sys.process._

        val versionFile = Paths.get("jvm", "src", "main", "resources", "versioninfo.properties")

        val gitRevision = ("git describe --tags --always".!!).trim
        val gitLastCommitTime = "git log -1 --format=%cd".!!
        val formatIn = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy ZZZZ", Locale.ENGLISH)
        val date = formatIn.parse(gitLastCommitTime)
        val formatOut = new java.text.SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH)

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

      },
      (compile in Compile) := ((compile in Compile) dependsOn versionFile).value,
      // do not run tests tagged as slow by default
//
//
//      // add a special config to run the slow tests:
//      // sbt slow:test
//
    )

lazy val replissJs: Project =
  repliss.js
    .enablePlugins(ScalablyTypedConverterPlugin)
    .settings(
      name := "replissJS",
      mainClass := Some("repliss.js.Main"),
      webpackDevServerPort := 8081,
      /* discover main and make the bundle run it */
      scalaJSUseMainModuleInitializer := true,
      //useYarn := true,
      /* disabled because it somehow triggers many warnings */
      scalaJSLinkerConfig := scalaJSLinkerConfig.value.withSourceMap(false).withModuleKind(CommonJSModule),
      resolvers += Resolver.bintrayRepo("hmil", "maven"),
      /* scala.js dependencies */
      libraryDependencies ++= Seq(
//        "me.shadaj" %% "slinky-core-ijext" % "0.6.6+19-e6702f29", // needs this for the intellij integration to work
//        "me.shadaj" %%% "slinky-web" % "0.6.6+19-e6702f29",
//        "me.shadaj" %%% "slinky-hot" % "0.6.6+19-e6702f29",
        "me.shadaj" %%% "slinky-core" % "0.6.6+19-e6702f29",
        "me.shadaj" %%% "slinky-web" % "0.6.6+19-e6702f29",
        "me.shadaj" %%% "slinky-core" % "0.6.6+19-e6702f29",
        "me.shadaj" %%% "slinky-hot" % "0.6.6+19-e6702f29",
//        "fr.hmil" %%% "roshttp" % "2.2.4",
//        "org.scala-js" %%% "scalajs-dom" % "0.9.7",
        "org.scala-js" %%% "scalajs-dom" % "1.1.0",
//        "com.lihaoyi" %%% "scalarx" % "0.4.0",
        "com.lihaoyi" %%% "scalarx" % "0.4.3",
        "org.scalatest" %%% "scalatest" % "3.2.3" % Test,
      ),
      /* javascript dependencies */
      Compile / npmDependencies ++= Seq(
        "react" -> "16.8.6",
        "@types/react" -> "16.9.53",
        "react-dom" -> "16.8.6",
        "@types/react-dom" -> "16.9.8",
        "react-proxy" -> "1.1.8",
        "@types/npm" -> "2.0.31",
      ),
      /* custom webpack file */
      Compile / webpackConfigFile := Some((ThisBuild / baseDirectory).value / "custom.webpack.config.js"),
      Compile / fastOptJS / webpackDevServerExtraArgs += "--mode=development",
      /* dependencies for custom webpack file */
      Compile / npmDevDependencies ++= Seq(
//        "file-loader" -> "3.0.1",
//        "style-loader" -> "0.23.1",
//        "css-loader" -> "2.1.1",
//        "html-webpack-plugin" -> "3.2.0",
        "copy-webpack-plugin" -> "5.0.2",
//        "webpack-merge" -> "4.2.1",
        "brace" -> "0.11.1",
        "svg2pdf.js" -> "1.3.4",
        // new:
        "webpack-merge" -> "5.2.0",
        "css-loader" -> "5.0.0",
        "style-loader" -> "2.0.0",
        "file-loader" -> "6.1.1",
        "url-loader" -> "4.1.1",
        "html-webpack-plugin" -> "4.5.0",
      ),
      // ignore missing types:
      stIgnore += "react-proxy",
      stIgnore += "svg2pdf.js",
      /* don't need to override anything for test. revisit this if you depend on code which imports resources,
          for instance (you probably shouldn't need to) */
      Test / webpackConfigFile := Some((ThisBuild / baseDirectory).value / "custom.webpack.config.js"),
      Test / npmDependencies ++= Seq(
        "source-map-support" -> "0.5.19"
      ),
      Test / requireJsDomEnv := true,
      stIgnore += "source-map-support",
      stFlavour := Flavour.Slinky,
      stReactEnableTreeShaking := Selection.All,
      scalacOptions += "-Ymacro-annotations",
//      scalacOptions += "-P:scalajs:sjsDefinedByDefault",
//      addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full),
    )

def cmd(name: String, commands: String*) =
  Command.command(name)(s => s.copy(remainingCommands = commands.toList.map(cmd => Exec(cmd, None)) ++ s.remainingCommands))

commands ++= List(
  cmd("dev", "fastOptJS::startWebpackDevServer", "~;replissJVM/reStart --server;replissJS/fastOptJS::webpack"),
  cmd("devFront", "fastOptJS::startWebpackDevServer", "~replissJS/fastOptJS::webpack"),
  cmd("devBack", "~;replissJVM/reStart --server"),
)




// Task for building docker image
lazy val docker = taskKey[Unit]("Build Docker image")
docker := {
  import sys.process._
  "docker build -t peterzel/repliss .".!
}

scalacOptions += "-Yrangepos"

// Do not run tests for assembly task
test in assembly := {}