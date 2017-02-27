name := "observatory"

scalaVersion := "2.11.8"

libraryDependencies := Seq(
  "org.typelevel" %% "cats" % "0.9.0",
  "co.fs2" %% "fs2-core" % "0.9.4",
  "co.fs2" %% "fs2-io" % "0.9.4",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)
