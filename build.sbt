lazy val root = (project in file(".")).
  settings(
    name := "anna",
    version := "1.0",
    scalaVersion := "2.13.18",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
  )

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.8.8",
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