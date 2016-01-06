package ru.wordmetrix.treeapproximator

import java.io.File

import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.utils.Use.anyToUse
import ru.wordmetrix.utils.{CFG, log}
import ru.wordmetrix.vector.Vector

class ArrangeTextDumpClusters(arrangetree: ArrangeText)(implicit cfg: CFG)
  extends ArrangeTextDump(arrangetree) {

  type Word = Int
  type Node = TreeApproximator.Node[Word, File]
  type Tree = TreeApproximator.Tree[Word, File]
  type Leaf = TreeApproximator.Leaf[Word, File]

  val tree: Tree = arrangetree.tree_aligned
  val map: Iterable[Iterable[Vector[Word]]] = arrangetree.clusters
  val path = cfg.path

  def dump() = {
    val v2f = tree.toMap
    val average = tree.average.normal
    map.zipWithIndex foreach {
      case (vs, i) =>
        val centroid_delta = vs.reduce(_ + _).normal - average.normal
        val path1 = path / "%04d : %s".format(
          i,
          vector2Title(centroid_delta)
        )

        path1 / "vocabulary.txt" write (centroid_delta)

        vs.zipWithIndex foreach {
          // TODO: The vector is lost sometimes
          case (v, j) => v2f.get(v) use {
            case None =>
              println("We met a problem with v: " + v)
              v2f.keys.maxBy(x => v * x) match {
                case x => {
                  log(s"The best solution is x: $x")
                  log("that is as good as " + x * v + " "
                    + x.normal * v.normal
                    + " " + (x - v).norm)
                }
              }
            case Some(x) =>
              x.copyTo(path1 / "%03d-%s".format(j, x.getName()))
          }
        }
    }
  }
}