lazy val commonSettings = Seq(
  organization := "com.leacox",
  version := "1.0",
  scalaVersion := "2.10.3"
)

lazy val root = (project in file("."))
    .settings(commonSettings: _*)
    .settings(
      name := "toml-scala",
      libraryDependencies += "joda-time" % "joda-time" % "2.7"
    )
