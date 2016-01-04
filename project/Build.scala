import sbt._

object WebCrawlerBuild extends Build {
  val Name = "webcrawler"
  val utils =
    RootProject(uri("https://github.com/electricmind/utils.git#scala-2_11"))

  override lazy val settings = super.settings

  lazy val root = Project(Name,
    base = file("."),
    settings = Project.defaultSettings
  ).dependsOn(utils)
}

