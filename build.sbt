lazy val root = (project in file(".")).
  settings(
    name := "anna",
    version := "1.0",
    scalaVersion := "2.11.6"
  )
    
ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.10"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.9",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.typesafe" % "config" % "1.2.1",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")