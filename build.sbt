import scala.sys.process._

ThisBuild / version := "0.1.0"
ThisBuild / organization := "codes,quine"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / scalacOptions ++= Seq(
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

lazy val testSettings = Seq(
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6" % Test,
  testFrameworks += new TestFramework("utest.runner.Framework"),
  doctestTestFramework := DoctestTestFramework.MicroTest
)

lazy val root = (project in file("."))
  .aggregate(core, magnolia, docs)
  .settings(name := "leanprops")

lazy val core = (project in file("modules/core"))
  .settings(name := "leanprops-core", testSettings)

lazy val magnolia = (project in file("modules/magnolia"))
  .settings(
    name := "leanprops-magnolia",
    libraryDependencies += "com.propensive" %% "magnolia" % "0.10.0",
    testSettings
  )
  .dependsOn(core)

lazy val docs = (project in file("website"))
  .settings(
    name := moduleName.value,
    moduleName := "leanprops-docs",
    mdocVariables := Map(
      "VERSION" -> version.value
    ),
    scalafmt := {
      Process("yarn format", Option(baseDirectory.value)) ! streams.value.log
    },
    scalafmt / test := {
      val exitCode = Process("yarn lint", Option(baseDirectory.value)) ! streams.value.log
      if (exitCode != 0)
        throw new Exception(s"Process returned exit code: $exitCode")
    }
  )
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
  .dependsOn(core, magnolia)
