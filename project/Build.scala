import sbt._

object TreeApproximatorBuild extends Build {
  val Name = "treeapproximator_root"
  val utils =
    RootProject(uri("https://github.com/electricmind/utils.git#scala-2_11"))

  override lazy val settings = super.settings

  val treeapproximator =
    Project(id="treeapproximator", base=file("treeapproximator")).dependsOn(utils)

  val draw2dmap =
    Project(id="draw2dmap", base=file("draw2dmap")).dependsOn(utils, treeapproximator)

  val arrangetext =
    Project(id="arrangetext", base=file("arrangetext")).dependsOn(utils, treeapproximator)

  lazy val root = Project(Name,
    base = file("."),
    settings = Project.defaultSettings
  ).dependsOn(utils).aggregate(draw2dmap, treeapproximator, arrangetext)
}
