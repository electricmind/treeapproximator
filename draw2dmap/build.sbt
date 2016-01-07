name := "draw2dmap"

version := "1.3"

scalaVersion := "2.11.7"

mainClass := Some("ru.wordmetrix.draw2dmap.Draw2DMap")

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"

libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.5"

libraryDependencies +=  "com.typesafe.akka" %% "akka-actor" % "2.4.1"

libraryDependencies +=  "com.typesafe.akka" %% "akka-testkit" % "2.4.1" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies +=  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
