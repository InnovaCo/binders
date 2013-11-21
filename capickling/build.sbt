name := "capickling"

version := "0.0-SNAPSHOT"

organization := "eu.inn"

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "com.datastax.cassandra" % "cassandra-driver-core" % "2.0.0-beta1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.RC1" % "test"

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0"

addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)