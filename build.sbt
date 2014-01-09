name := "treeapproximator"

version := "1.0"

scalaVersion := "2.10.2"

//mainClass := Option("ru.wordmetrix.WholeLotOfPictures")
mainClass := Option("ru.wordmetrix.webcrawler.WebCrawler")

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.netflix.rxjava" % "rxjava-scala" % "0.15.1"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

//libraryDependencies += "se.fishtank" % "css-selectors-scala_2.9.1" % "0.1.2"

libraryDependencies += "commons-codec" % "commons-codec" % "1.2"
            
//resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

//org.scalastyle.sbt.ScalastylePlugin.Settings

//import de.johoop.findbugs4sbt.FindBugs._

//findbugsSettings

