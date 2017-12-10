name := "observatory"

scalaVersion := "2.12.3"

scalafmtVersion in ThisBuild := "1.3.0"

val akkaVersion = "2.5.8"
libraryDependencies := Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.typelevel" %% "cats-kernel" % "1.0.0-MF",
  "org.scalacheck"             %% "scalacheck"     % "1.13.4" % "test",
  "ch.qos.logback"             % "logback-classic" % "1.2.3",
  "com.ironcorelabs" %% "cats-scalatest" % "2.3.0" % "test",
  "com.typesafe.scala-logging" %% "scala-logging"  % "3.7.2"
)
