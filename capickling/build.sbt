name := "capickling"

version := "0.0-SNAPSHOT"

organization := "eu.inn"

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"

libraryDependencies += "com.datastax.cassandra" % "cassandra-driver-core" % "2.0.0-beta1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.RC1" % "test"

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" % "test"

addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)