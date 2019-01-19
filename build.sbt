lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.12.8",
  organization := "codes.quine",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:nullary-override",
    "-Xlint:nullary-unit",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Xlint:unsound-match",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  )
)

lazy val testSettings = Seq(
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  doctestTestFramework := DoctestTestFramework.MicroTest,
  coverageEnabled := true
)

lazy val root = (project in file("."))
  .aggregate(core, magnolia)
  .settings(commonSettings, name := "leanprops")

lazy val core = (project in file("modules/core"))
  .settings(commonSettings, name := "leanprops-core", testSettings)

lazy val magnolia = (project in file("modules/magnolia"))
  .settings(
    commonSettings,
    name := "leanprops-magnolia",
    libraryDependencies += "com.propensive" %% "magnolia" % "0.10.0",
    testSettings
  )
  .dependsOn(core)
