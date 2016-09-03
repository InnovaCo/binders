import sbt.Keys._

name := "binders-core"

version := "0.13.0-SNAPSHOT"

organization := "eu.inn"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.10.6")

libraryDependencies ++= Seq(
  "org.mockito" % "mockito-all" % "1.10.19" % "test",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value
    // in Scala 2.10, quasiquotes are provided by macro paradise
    case Some((2, 10)) =>
      libraryDependencies.value ++ Seq(
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
        "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary)
  }
}
