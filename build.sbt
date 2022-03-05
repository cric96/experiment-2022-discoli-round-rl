ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "experiment-discoli-ac-round-rl"
  )

libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.3.0"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.0"
libraryDependencies += "com.outr" %% "scribe" % "3.8.0"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10"
libraryDependencies += "com.lihaoyi" %% "upickle" % "1.4.3"

addCommandAlias("startSimulation", "runMain it.unibo.casestudy.launch.Main config.json")
