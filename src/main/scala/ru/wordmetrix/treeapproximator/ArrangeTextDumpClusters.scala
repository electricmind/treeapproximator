package ru.wordmetrix.treeapproximator

import ru.wordmetrix.utils.CFG
import java.io.File

import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.util.matching.Regex

import impl._
import ru.wordmetrix.smartfile.SmartFile.{ fromFile, fromString, toFile }
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.utils.log
import ru.wordmetrix.utils.Use.anyToUse
import ru.wordmetrix.vector.Vector

class ArrangeTextDumpClusters(arrangetree: ArrangeText)(implicit cfg: CFG)
        extends ArrangeTextDump(arrangetree) {

    val tree: Tree = arrangetree.tree
    val map: Iterable[Iterable[Vector[Word]]] = arrangetree.clusters
    val path = cfg.path

    def arrange_cluster() = {
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
                                case x =>
                                    {
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