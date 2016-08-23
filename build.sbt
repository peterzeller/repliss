name := "crdt-verify-scala"

version := "1.0"

scalaVersion := "2.11.8"


//libraryDependencies += "org.scalameta" %% "scalameta" % "1.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"


antlr4Settings
antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5.2"
antlr4PackageName in Antlr4 := Some("crdtver.parser")
antlr4GenVisitor in Antlr4 := true
//javaSource in Antlr4 := (baseDirectory / "src-gen").value
//
//unmanagedSourceDirectories in Compile += baseDirectory.value / "src-gen"


libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"