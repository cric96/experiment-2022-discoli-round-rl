ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "experiment-discoli-ac-round-rl"
  )

libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.3.0"