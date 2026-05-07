val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "com.github.tototoshi" %% "scala-csv" % "1.3.10",
      "org.scalameta" %% "scalameta" % "4.15.2"
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
   Test / parallelExecution := false,
Test / logBuffered := false,
Test / testOptions ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
  Tests.Argument(TestFrameworks.ScalaTest, "-C", "CustomReporter")
)
  )
