import sbt._
import Keys._

object WebCrawlerBuild extends Build {
    override lazy val settings = super.settings 
    lazy val root = Project("webcrawler", //id = "webcrawler",
                            base = file("."),
                            settings = Project.defaultSettings 
    ).dependsOn(
      smartfile
    )

    lazy val smartfile  = RootProject(uri("https://github.com/electricmind/smartfile.git"))
}

