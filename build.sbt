// Turn this project into a Scala.js project by importing these settings
scalaJSSettings

name := "Floppy Bird Scala.js"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    "org.scala-lang.modules.scalajs" %% "scalajs-dom" % "0.3",
    "org.scala-lang.modules.scalajs" %% "scalajs-jquery" % "0.3"
)
