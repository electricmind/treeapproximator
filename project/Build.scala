import sbt._
import Keys._

object WebCrawlerBuild extends Build {
    override lazy val settings = super.settings 
    lazy val root = Project("webcrawler", //id = "webcrawler",
                            base = file("."),
                            settings = Project.defaultSettings 
    ).dependsOn(
      utils
    )

    lazy val utils  = RootProject(uri("https://github.com/electricmind/utils.git#7dfed4c4b39e3894f66520bd18d5048e2b0805b3"))
}

