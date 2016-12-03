lazy val root = (project in file(".")).
  settings(
    name := "anna",
    version := "1.0",
    scalaVersion := "2.11.8"
  )

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.14",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.14",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.typesafe" % "config" % "1.3.1",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.json4s" %% "json4s-native" % "3.3.0"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

initialCommands in console :=
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