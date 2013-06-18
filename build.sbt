name := "HelloAkka"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.14" % "test",
			    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
			    "com.typesafe.akka" %% "akka-actor" % "2.1.4")

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")