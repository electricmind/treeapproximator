package ru.wordmetrix.treeapproximator

import java.io.File
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.util.Random
import scala.xml.{ Text, Unparsed }
import ru.wordmetrix.smartfile.SmartFile.{ fromFile, fromString, toFile }
import ru.wordmetrix.treeapproximator.TreeApproximator.{ Leaf, Node, Tree }
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.utils.Use.anyToUse
import ru.wordmetrix.utils.debug
import ru.wordmetrix.vector.Vector
import scala.util.matching.Regex
import scala.util.Try
import ru.wordmetrix.utils._
import scala.annotation.tailrec
import ru.wordmetrix.features.Features.String2Word
import ru.wordmetrix.features.Features
import ru.wordmetrix.vector.VectorHASH
import ru.wordmetrix.vector.VectorList

/**
 *  ArrangeText is a strategy that places a bunch of text in convenient fashion.
 *
 *  It provides three predefined approaches of saving text: a tree of text
 *  arranged by similarity, a set of clusters placed into folders and a web-
 *  page of links on web pages (assuming that an original URI is available).
 */
object ArrangeText extends App {
    override def main(args: Array[String]) {
        val (command, args1) = args match {
            case Array(command, args @ _*) if Set("tree", "cluster", "links")(command) =>
                (Some(command), args.toList)

            case args @ Array(arg, _@_*) => (Some("all"), args.toList)

            case args =>
                println("\nEnter: (tree | cluster | links) [Options] [<FILE> [..]]\n")
                (None, args.toList)
        }

        implicit val cfg = CFG(args1)

        lazy val arrangetext = ArrangeText()

        command foreach {
            case "tree" =>
                new ArrangeTextDumpTree(arrangetext).dump() //arrange_tree(tree_aligned, target)

            case "cluster" =>
                new ArrangeTextDumpClusters(arrangetext).dump()

            case "links" =>
                new ArrangeTextDumpHTML(arrangetext, cfg.f2u match {
                    case CFG.F2U.Simple => new File2URITransform()
                    case CFG.F2U.Map    => new File2URIMap()
                    case CFG.F2U.Dump   => new File2URIDump()
                }).dump()

            case "all" =>
                println("tree.size = " + arrangetext.tree.size)
                new ArrangeTextDumpTree(arrangetext).dump() //arrange_tree(tree_aligned, target)

                println("cluster suze = " + arrangetext.clusters.size)
                new ArrangeTextDumpClusters(arrangetext).dump()

                println("links = " + arrangetext.clusters.size)

            case _ => println("Huh, boyz ...")
        }
    }

    def apply()(implicit cfg: CFG) = new ArrangeText()
}

abstract class ArrangeTextDump(arrangetree: ArrangeText)(implicit cfg: CFG) {
    implicit def accuracy = cfg.accuracy

    def vector2Title(v: Vector[String], n: Int = 5, stopword: Set[String] = Set(" ")) = {
        v.toList.sortBy(-_._2).takeWhile(_._2 > 0d).map(_._1).filterNot(stopword).filterNot(Set(" ", "")).take(n).mkString(" ")
    }

    implicit def vectors2Vectors(v: Vector[Word]): Vector[String] = Vector(v.map {
        case (x, y) => (arrangetree.index.rmap.getOrElse(x, "unknown" /*"Word is unknown or index possibly is old"*/ ) -> y)
    } toList)
}

class ArrangeText()(implicit cfg: CFG) {
    implicit def accuracy = cfg.accuracy

    type Word = Int
    type Node = TreeApproximator.Node[Word, File]
    type Tree = TreeApproximator.Tree[Word, File]
    type Leaf = TreeApproximator.Leaf[Word, File]

    val start_time = System.currentTimeMillis()

    def tree_raw: Tree = vectors.zipWithIndex.foldLeft(TreeApproximator[Word, File]())({
        case (tree, ((vector, file), n)) => log.time("%s %d %s tree(%s).energy => %4.3f, length = %d / %d".format(
            (System.currentTimeMillis() - start_time),
            tree.n,
            index.map.size,
            file,
            tree.energy2,
            vector.size,
            tree.average.size)
        ) {
            if (n % 100 == 0) System.gc()
            (tree + (vector, file)).rectify(cfg.rectify_inline)
        }
    })

    def tree_opt: Tree = (1 to cfg.rectify).foldLeft(tree_raw)({
        case (tree, n) =>
            log.time("Rectifying #%3d = %4.3f %d, size = %s, length = %s".format(
                n, tree.energy2, tree.average.size, tree.toList.length, 
                tree.align()._1.pathlength)
            ) {
                tree.rectify(tree.n)
            }
    })

    def tree_aligned = (cfg.path / "tree.dat") cache {
        val tree = tree_opt.align()._1
 //       debug("tree_aligned size = %s", tree_opt.toList.length)
        val size = tree.size
        val pathlength = tree.pathlength
        log("Average distance = %f (%f/%d)", pathlength/size, pathlength, size)
        tree
    }

    lazy val tree: Tree = tree_aligned

    lazy val index = cfg.path / "index.dat" cache index_uncached
    
    lazy val (vectors, index_uncached) = sample(
        for {
            file <- scala.util.Random.shuffle(cfg.files).toStream
            page <- Try(file.readLines().mkString(" ")).toOption
        } yield (
            if (cfg.ishtml) new Html2Ascii(page).wrap(72) else page,
            file
        ),
        Stream(), String2Word()
    ) match {
            case (vectors, index) =>
                val (empties, substantative) = vectors.partition({
                    case (vs, filename) => vs.isEmpty
                })

                if (empties.nonEmpty)
                    log("%s was rejected as empty", empties.size)

                if (cfg.isdebug) empties.foreach {
                    case (v, file) => debug("Splitter reduces %s to ashes", file)
                }

                val uniq = substantative.toMap

                debug("%s uniq vectors", uniq.size)

                if (uniq.size != substantative.size) {
                    log("%s duplicates were rejected",
                        substantative.size - uniq.size)
                        
                    if (cfg.isdebug)
                        substantative.filter({
                            case (v, f) => uniq.get(v) match {
                                case Some(`f`) => false
                                case _         => true
                            }
                        }
                        ) foreach {
                            case (v, f) =>
                                debug("%s is the same as %s", f, uniq(v))
                        }
                }

                cfg.path / "vocabulary_whole.txt" write index.map.keys.toList.sorted

                (uniq.toStream,  index)
        }

    lazy val clusters: Iterable[Iterable[Vector[Word]]] =
        debug.time("clustering") {
            debug("Size tree: %s", tree.toList.size)
            val c = Clusters(tree_aligned)
            debug("clusters %s %s %s", c.size, c.iterator.toList.flatten.size,
                c.heads.toList.flatMap{ _._2 }.size)
            c
        }

    val delimiter: Regex = """\W+""".r

    @tailrec
    private def sample(
        files: Stream[(String, File)],
        vectors: Stream[(Vector[Word], File)],
        index: String2Word[String, Double]): (Stream[(Vector[Word], File)], String2Word[String, Double]) =
        files match {
            case (s, file) #:: files =>
                val (countids, index1) = Features.fromText(s, index)
                sample(
                    files,
                    (Vector(countids.toList), file) #:: vectors,
                    index1

                )
            case Stream() => (vectors, index)
        }
}
