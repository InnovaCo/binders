name := "capickling-sample"

version := "0.0-SNAPSHOT"

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.file("Local Maven Repository", 
	file(Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)

libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"

libraryDependencies += "com.datastax.cassandra" % "cassandra-driver-core" % "2.0.0-beta1"

libraryDependencies += "eu.inn" %% "capickling" % "0.0-SNAPSHOT"