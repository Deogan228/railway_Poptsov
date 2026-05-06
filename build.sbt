ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "railway-monads",
    Compile / mainClass := Some("main"),
    Test / mainClass := Some("RailwayTests"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
  )
