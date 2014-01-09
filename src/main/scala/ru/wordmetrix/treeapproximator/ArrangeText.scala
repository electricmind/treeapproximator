package ru.wordmetrix.treeapproximator

import java.io.File
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.util.Random
import scala.xml.{ Text, Unparsed }
import ru.wordmetrix.smartfile.SmartFile.{ fromFile, fromString, toFile }
import ru.wordmetrix.treeapproximator.TreeApproximator.{ Leaf, Node, Tree }
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.utils.{CFG, CFGAware, log, debug}
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug
import ru.wordmetrix.utils.Use._

object ArrangeText extends App {
    implicit lazy val cfg = CFG(List("-d"))
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

    def arrange_cluster_into_jquery_ui(map: Iterable[Iterable[Vector[Word]]], tree: Tree[Word, File], path: File) =
        {
            val v2f = tree.toMap
            val average = tree.average.normal
            val clusters = map.zipWithIndex.toList

            <html lang="en">
                <head>
                    <meta charset="utf-8"/>
                    <title>Clusters of Neighbours of { root.getName() }</title>
                    <link rel="stylesheet" href="http://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css"/>
                    <script src="http://code.jquery.com/jquery-1.9.1.js"></script>
                    <script src="http://code.jquery.com/ui/1.10.3/jquery-ui.js"></script>
                    <link rel="stylesheet" href="/resources/demos/style.css"/>
                    <script>{
                        Unparsed("""
                            $(function(){
                                $("a[target=_blank]").click(function (event, ui) {
                                    var href = $(event.target).attr("href");
                                    $( "#dialog-confirm" ).dialog({
                                        modal: true,
                                        buttons: {
                                            "Open": function() {
                                                window.open(href,              
                                                    "popupWindow",
                                                    "scrollbars=yes"
                                                );
                                                $( this ).dialog( "close" );
                                            },
                                
                                            "Cancel": function() {
                                                $( this ).dialog( "close" );
                                            }
                                        },
                                        open: function(event, ui) {
                                            $(this).parent().css('position', 
                                            'fixed').css('top','10%');
                                            }

                                    });
                                    return false;
                                });
                                
                                $("#keywords").resizable();
                                $("#accordion").accordion({ 
                                    heightStyle: "content",
                                    activate : function(event, ui) {
                                      $("#keywords div.keyword").hide();
                                      $($(ui.newHeader).data("id")).show();
                                      $(window).scrollTop($(ui.newHeader).position().top-$("#header").height());
                                    }
                                });
                                
                                function resize() { 
                                    $("#wiki").width($(window).width()-210);
                                    $("#wiki").css("top", $("#header").height());
                                    $("#top").height($("#header").height()); 
                                }
                                $(window).resize(resize);  
                                resize();
                                setTimeout(function() {
                                    $("#accordion").accordion(
                                       'option',
                                       'active',
                                       $("#accordion ul").index(
                                            $($('a[target="wiki"][href="' + $("h1 a").attr("href") + '"]').parent()[0]).parent()
                                       )
                                    )   
                                }, 500);
                            
                            });
                            """)
                    }</script>
                </head>
                <body>
                    <div id="header" style="position:fixed; z-index:100; background:white; top:0px">
                        <h1> Clusters of Neighbours of <a href={ "http://en.wikipedia.org/wiki/" + root.getParent().getName() }>{
                            root.getParent().getName()
                        } </a> </h1>
                        <p>
                            This is an outcome of an algorithm that clusters
                            { v2f.size }
                            of pages, closest to the word
                            "{ root.getParent().getName() }
                            ".
                            Each of the
                            { clusters.length }
                            groups below contains a few links that 
                            point out to the wikipedia pages with similar 
                            content. The words in the title describes  traits 
                            of the content, up to 100 words are printed out 
                            above wikipedia article when group is opened.
                        </p>
                    </div>
                    <div id="top" style="height:150px"> </div>
                    <div id="accordion" style="width:185px; font-size:0.7em"> {
                        clusters map {
                            case (vs, i) => {
                                val centroid_delta = vs.reduce(_ + _).normal - average.normal
                                <h3 data-id={ "#keyword" + i }> {
                                    vector2Title(centroid_delta)
                                } </h3>
                                <div><ul> {
                                    vs.zipWithIndex map {
                                        case (v, j) => v2f.get(v) use {
                                            case Some(x) =>
                                                <li>
                                                    {
                                                        val href = x.getName().split("-") match {
                                                            case Array(x, y, z)=> Text("http://" + x + "/" + y + "/" + z)
                                                            case Array(x, y, z1, z2)=> Text("http://" + x + "/" + y + "/" + z1 + ":" + z2)
                                                            case Array(x, y, ls @ _*)=> Text("http://" + x + "/" + y + "/" + ls.dropRight(1).mkString(":") + "/" + ls.last)
                                                        }
                                                        val isframe = x.getName().split("-") match {
                                                            case Array(x, y, "Special", _@ _*)=> false
                                                            case _=> true
                                                        }
                                                        if (isframe)
                                                            <a target="wiki" href={ href }> {
                                                                x.getName().split("-").drop(2).map {
                                                                    _.replace("_", " ")
                                                                }
                                                            } </a>
                                                        else
                                                            <a target="_blank" href={ href } color="gray"> {
                                                                x.getName().split("-").drop(2).map {
                                                                    _.replace("_", " ")
                                                                }
                                                            } </a>

                                                    }
                                                </li>
                                        }
                                    }
                                } </ul> </div>
                            }
                        }
                    } </div>
                    <div id="wiki" style="position:fixed;top:200px;left:200px; width:80%; height:100%">
                        <div id="keywords" class="ui-widget-content" style="font-size: 0.7em; overflow:hidden">
                            <h3 class="ui-widget-header">Keywords</h3>{
                                clusters map {
                                    case (vs, i) =>
                                        <div id={ "keyword" + i } class="ui-helper-hidden keyword">
                                            { vs.reduce(_ + _).self.sortBy(-_._2).takeWhile(_._2 > 0).take(100).map(x => inverted(x._1)).sorted.mkString(" ") }
                                        </div>
                                }
                            }
                        </div>
                        <br/>
                        <iframe src={ "http://en.wikipedia.org/wiki/" + root.getParent().getName() } height="100%" width="100%" name="wiki"></iframe>
                    </div>
                    <hr/>
                    <p>This page created with jquery-ui :) </p>
                    <div id="dialog-confirm" title="Open new window?">
                        <p>
                            <span class="ui-icon ui-icon-alert" style="float: left; margin: 0 7px 20px 0;"></span>
                            For the sake of Wikipedia restrictions the page can be opened only in new window.
                            It's quite inconvenient. Are you sure?
                        </p>
                    </div>
                </body>
            </html>

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
                        arrange_cluster_into_jquery_ui(debug.time("clustering") { Clusters(tree) }, tree, target).toString
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
                                arrange_cluster_into_jquery_ui(c, tree, target).toString
                            )
                            arrange_tree(tree, target / "tree")
                        }
                }

            case _ => println("Huh, boyz ...")
        }
    }
}
