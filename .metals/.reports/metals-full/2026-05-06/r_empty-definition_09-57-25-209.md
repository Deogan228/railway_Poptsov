error id: file:///C:/Users/rusla/OneDrive/Рабочий%20стол/ФП/railway-monads/build.sbt:`<error>`#`<error>`.
file:///C:/Users/rusla/OneDrive/Рабочий%20стол/ФП/railway-monads/build.sbt
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -Test.
	 -Test#
	 -Test().
	 -scala/Predef.Test.
	 -scala/Predef.Test#
	 -scala/Predef.Test().
offset: 204
uri: file:///C:/Users/rusla/OneDrive/Рабочий%20стол/ФП/railway-monads/build.sbt
text:
```scala
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "railway-monads",
    Compile / mainClass := Some("main"),
    @@Test / mainClass := Some("RailwayTests"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
  )

```


#### Short summary: 

empty definition using pc, found symbol in pc: 