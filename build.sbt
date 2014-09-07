import sbt.Keys._

name := "binders-core"

version := "0.2.1"

organization := "eu.inn"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.11.2", "2.10.4")

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies <+= scalaVersion(scalatestDependency(_))

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

def scalatestDependency(scalaVersion: String) = scalaVersion match {
  case s if s.startsWith("2.10.") => "org.scalatest" % "scalatest_2.10" % "2.2.0" % "test"
  case s if s.startsWith("2.11.") => "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
}

// Sonatype repositary publish options
publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false}

pomExtra := (
  <url>https://github.com/InnovaCo/binders</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://opensource.org/licenses/BSD-3-Clause</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:InnovaCo/binders.git</url>
      <connection>scm:git:git@github.com:InnovaCo/binders.git</connection>
    </scm>
    <developers>
      <developer>
        <id>InnovaCo</id>
        <name>Innova Co S.a r.l</name>
        <url>https://github.com/InnovaCo</url>
      </developer>
      <developer>
        <id>maqdev</id>
        <name>Maga Abdurakhmanov</name>
        <url>https://github.com/maqdev</url>
      </developer>
    </developers>
  )