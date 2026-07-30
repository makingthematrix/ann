val _scalaVersion = "3.8.4"

name := "anna"
ThisBuild / licenses := Seq("GPL 3.0" -> url("https://www.gnu.org/licenses/gpl-3.0.en.html"))
ThisBuild / scalaVersion := _scalaVersion
ThisBuild / versionScheme := Some("semver-spec")
Test / scalaVersion := _scalaVersion
ThisBuild / description := "An artificial neural network as a data flow transformer"

val standardOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding",
  "utf8"
)

val scala3Options = Seq(
  "-explain",
  "-Wsafe-init",
  "-Ycheck-all-patmat",
  "-Wunused:imports",
  "-no-indent", "-rewrite"
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/makingthematrix/signals3"),
    "scm:git:git@github.com:makingthematrix/signals3.git"
  )
)

developers := List(
  Developer(
    "makingthematrix",
    "Maciej Gorywoda",
    "makingthematrix@protonmail.com",
    url("https://github.com/makingthematrix"))
)


lazy val root = (project in file(".")).
  settings(
    name := "anna",
    version := "1.1",
    scalaVersion := _scalaVersion,
    scalacOptions ++= standardOptions ++ scala3Options
  )

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.8.8",
  "com.typesafe.akka" %% "akka-actor-typed" % "2.8.8",
  "com.typesafe.akka" %% "akka-testkit" % "2.8.8",
  "org.scalatest" %% "scalatest" % "3.2.20" % "test",
  "com.typesafe" % "config" % "1.4.9",
  "junit" % "junit" % "4.13.2" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.json4s" %% "json4s-native" % "4.0.7"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

initialCommands / console :=
  """
    |import anna.Commands._
    |import anna.logger.LOG
    |import anna.Context
    |import anna.utils.DoubleRange
    |import anna.utils.DoubleRange._
    |import anna.utils.RandomNumber
    |import anna.async.NeuronCounter
    |LOG.addLogToStdout()
    |println("Type 'help' for instructions")
  """.stripMargin