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
import impl._

/**
 *  ArrangeText is a strategy that places a bunch of text in convenient fashion.
 *
 *  It provides three predefined approaches of saving text: a tree of text
 *  arranged by similarity, a set of clusters placed into folders and a web-
 *  page of links on web pages (assuming that an original URI is available).
 */
object ArrangeText extends App {
    implicit lazy val cfg = CFG(isdebug = true)
    type Word = Int
    implicit lazy val accuracy: Double = 0.01

    object string2word {
        var map = Map[String, Word]()
        val words = Iterator.from(10)
        var inverted = Map[Word, String]()
        def apply(s: String) = {
            val word = map.getOrElse(s, words.next)
            map += (s -> word)
            inverted += (word -> s)
            word
        }
    }

    var root = "/tmp"
    lazy val inverted = root / "word2string.dat" cache {
        string2word.inverted
    }
    implicit def string2File(s: String) = new File(s)
    implicit def vectors2Vectors(v: Vector[Word]): Vector[String] = Vector(v.map {
        case (x, y) => (inverted.getOrElse(x, "unknown" /*"Word is unknown or index possibly is old"*/ ) -> y)
    } toList)

    def vector2Title(v: Vector[String], n: Int = 5, stopword: Set[String] = Set(" ")) = {
        v.toList.sortBy(-_._2).takeWhile(_._2 > 0d).map(_._1).filterNot(stopword).filterNot(Set(" ", "")).take(n).mkString(" ")
    }

    def arrange_tree(tree: Tree[Word, File], path: File): Unit = tree match {
        case node: Node[Word, File] => {
            val stopword = "[\\W+]".r.split(path.toString).map(_.trim).toSet
            val centroid_delta_1 = node.child1.average.normal - node.child2.average.normal
            val path1 = path / (if (path.toString.length > 1000) "1" else "1 : %s".format(vector2Title(centroid_delta_1, 3, stopword)))

            path1 / "vocabulary.txt" write (centroid_delta_1)
            arrange_tree(node.child1, path1)

            val centroid_delta_2 = node.child2.average.normal - node.child1.average.normal
            val path2 = path / (if (path.toString.length > 1000) "2" else "2 : %s".format(vector2Title(centroid_delta_2, 3, stopword)))

            path2 / "vocabulary.txt" write (centroid_delta_2)
            arrange_tree(node.child2, path2)
        }

        case leaf: Leaf[Word, File] =>
            leaf.value.copyTo(path / leaf.value.toString)
    }

    def arrange_cluster(map: Iterable[Iterable[Vector[Word]]], tree: Tree[Word, File], path: File) = {
        val v2f = tree.toMap
        val average = tree.average.normal
        map.zipWithIndex foreach {
            case (vs, i) =>
                val centroid_delta = vs.reduce(_ + _).normal - average.normal
                val path1 = path / "%04d : %s".format(i, vector2Title(centroid_delta))
                path1 / "vocabulary.txt" write (centroid_delta)
                vs.zipWithIndex foreach {
                    // TODO: The vector is lost sometimes
                    case (v, j) => v2f.get(v) use {
                        case None =>
                            println("We met a problem with v: " + v)
                            v2f.keys.maxBy(x => v * x) use {
                                x =>
                                    {
                                        println("The best solution is x: " + x)
                                        println("that is as good as " + x * v + " " + x.normal * v.normal + " " + (x - v).norm)
                                    }
                            }
                        case Some(x) => x.copyTo(path1 / "%03d-%s".format(j, x.getName()))
                    }
                }
        }
    }

    override def main(args: Array[String]) {
        val delimiter = """\W+""".r

        val (command, target, files) = args match {
            case Array(command, target, files @ _*) if Set("tree", "cluster", "links")(command) => (command, target, files)

            case Array(target, files @ _*) => ("both", target, files)

            case _ =>
                println("\nEnter: tree | cluster <PATH> [<FILE> [..]]\n")
                ("nothing", ".", Seq[String]())
        }
        root = target

        def vectors = Random.shuffle(files).toIterator.map(x => new File(x)).map(x => try {
            Some((Vector(
                x.readLines().map(delimiter.split).flatten
                    .toList.groupBy(x => x.toLowerCase())
                    .map({ case (x, y) => (x, y.length.toDouble) })
                    .filter(_._2 > 5)
                    .filter(_._1.length > 3)
                    .map({ case (x, y) => string2word(x) -> y })
                    .toList), x)
            )
        } catch {
            case x: Throwable => println("File open failure: " + x); None
        }).flatten

        val t = System.currentTimeMillis()
        def tree = vectors.zipWithIndex.foldLeft(TreeApproximator[Word, File]())({
            case (tree, ((vector, file), n)) => {
                debug.time("%s %d %s tree(%s).energy => %4.3f, length = %d / %d".format(
                    (System.currentTimeMillis() - t),
                    tree.n,
                    string2word.map.size,
                    file,
                    tree.energy2,
                    vector.size,
                    tree.average.size)
                ) {
                    if (n % 100 == 0) System.gc()
                    (tree + (vector, file)).rectify(2)
                }

            }
        })

        def tree_opt = (1 to 5).foldLeft(tree /*.asInstanceOf[Node[Word, File]]*/ )({
            case (tree, n) =>
                debug.time("Rectifying #%3d = %4.3f %d".format(
                    n, tree.energy2, tree.average.size)
                ) {
                    tree.rectify(tree.n)
                }
        })

        def tree_aligned = (target / "tree.dat") cache {
            val tree = tree_opt.align()._1
            tree
        }

        command match {
            case "tree" => arrange_tree(tree_aligned, target)
            case "cluster" => tree_aligned use {
                tree => arrange_cluster(debug.time("clustering") { Clusters(tree) }, tree, target)
            }
            case "links" => tree_aligned use {
                tree =>
                    target / "index.html" write (
                        debug.time("clustering") { Clusters(tree) }
                             .arrange_cluster_into_jquery_ui(tree.toMap, tree.average.normal, root.getParent().getName(), target).toString
                    )
            }
            case "both" =>
                tree_aligned use {
                    tree =>
                        {
                            println("tree.size = " + tree.size)
                            val c = debug.time("clustering") { Clusters(tree) }
                            println("cluster suze = " + c.size)
                            arrange_cluster(c, tree, target / "cluster")
                            target / "index.html" write (
                                c.arrange_cluster_into_jquery_ui(tree.toMap, tree.average.normal, root.getParent().getName(), target).toString
                            )
                            arrange_tree(tree, target / "tree")
                        }
                }

            case _ => println("Huh, boyz ...")
        }
    }
}
