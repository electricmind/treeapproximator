import sbt._

object TreeApproximatorBuild extends Build {
  val Name = "treeapproximator"
  val utils =
    RootProject(uri("https://github.com/electricmind/utils.git#scala-2_11"))

  override lazy val settings = super.settings

  lazy val root = Project(Name,
    base = file("."),
    settings = Project.defaultSettings
  ).dependsOn(utils)
}

