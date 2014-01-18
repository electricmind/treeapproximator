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

            case args @ Array(arg, _@ _*) => (Some("all"), args.toList)

            case args @ _ =>
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
                new ArrangeTextDumpHTML(arrangetext).dump()

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
    
    def vector2Title(v: Vector[String], n: Int = 5, stopword: Set[String] = Set(" ")) = {
        v.toList.sortBy(-_._2).takeWhile(_._2 > 0d).map(_._1).filterNot(stopword).filterNot(Set(" ", "")).take(n).mkString(" ")
    }

    implicit def vectors2Vectors(v: Vector[Word]): Vector[String] = Vector(v.map {
        case (x, y) => (arrangetree.index.rmap.getOrElse(x, "unknown" /*"Word is unknown or index possibly is old"*/ ) -> y)
    } toList)
    
}

class ArrangeText()(implicit cfg: CFG) {
    type Word = Int
    type Node = TreeApproximator.Node[Word, File]
    type Tree = TreeApproximator.Tree[Word, File]
    type Leaf = TreeApproximator.Leaf[Word, File]

    val start_time = System.currentTimeMillis()

    lazy val tree: Tree = vectors.zipWithIndex.foldLeft(TreeApproximator[Word, File]())({
        case (tree, ((vector, file), n)) => debug.time("%s %d %s tree(%s).energy => %4.3f, length = %d / %d".format(
            (System.currentTimeMillis() - start_time),
            tree.n,
            index.map.size,
            file,
            tree.energy2,
            vector.size,
            tree.average.size)) {
            {
                if (n % 100 == 0) System.gc()
                (tree + (vector, file)).rectify(2)
            }
        }
    })

    lazy val (vectors, index) = sample(
        List(),
        List(), String2Word()
    )

    lazy val clusters: Iterable[Iterable[Vector[Word]]] =
        debug.time("clustering") {
            Clusters(tree)
        }
    
    lazy val clusters1 =
        debug.time("clustering") {
            Clusters(tree)
        }
    
    def tree_opt = (1 to 5).foldLeft(tree)({
        case (tree, n) =>
            debug.time("Rectifying #%3d = %4.3f %d".format(
                n, tree.energy2, tree.average.size)
            ) {
                tree.rectify(tree.n)
            }
    })

    def tree_aligned = (cfg.path / "tree.dat") cache {
        val tree = tree_opt.align()._1
        tree
    }

    case class String2Word(val map: Map[String, Int] = Map(),
                           val rmap: Map[Int, String] = Map(),
                           n: Int = 0) {
        def update(word: String) = copy(
            map = map + (word -> (n + 1)),
            rmap = rmap + ((n + 1) -> word),
            n + 1
        )
    }

    val delimiter: Regex = """\W+""".r

    def sample(
        files: List[(String, File)],
        vectors: List[(Vector[Word], File)],
        index: String2Word): (List[(Vector[Word], File)], String2Word) =
        files match {
            case (s, file) :: files =>
                val countwords = for {
                    (x, ys) <- (for {
                        word <- delimiter.split(s)
                        if word.length > cfg.wordlen
                    } yield word).groupBy(x => x.toLowerCase())

                    y <- Some(ys.toList.length.toDouble)
                    if y > cfg.wordfreq
                } yield { x -> y }

                val (countids, index1) = countwords.foldLeft(
                    Map[Word, Double](), index
                ) {
                        case ((map, index), (x, y)) =>
                            val index1 = index.update(x)
                            (map + (index.n -> y), index)
                    }

                sample(
                    files,
                    (Vector(countids.toList), file) :: vectors,
                    index1

                )
            case List() => (vectors, index)
        }

}

 /*   
    type Word = Int
    implicit lazy val accuracy: Double = 0.01


    var root = "/tmp"
    lazy val inverted = root / "word2string.dat" cache {
        string2word.inverted
    }
    implicit def string2File(s: String) = new File(s)

    implicit def vectors2Vectors(v: Vector[Word]): Vector[String] = Vector(v.map {
        case (x, y) => (inverted.getOrElse(x, "unknown" /*"Word is unknown or index possibly is old"*/ ) -> y)
    } toList)



    

    override def main(args: Array[String]) {

    }
}
*/

