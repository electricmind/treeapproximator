package ru.wordmetrix.treeapproximator

import java.io.{ File, FileInputStream }
import java.net.URI
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.util.{ Success, Try }
import scala.util.Failure
import scala.xml.{ Text, Unparsed }
import scala.xml.{ Text, Unparsed }
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.parsing.NoBindingFactoryAdapter
import ru.wordmetrix.smartfile.SmartFile.{ fromFile, fromString, toFile }
import ru.wordmetrix.treeapproximator.TreeApproximator.Node
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.utils.Use.anyToUse
import ru.wordmetrix.utils.log
import ru.wordmetrix.vector.Vector
import sun.misc.BASE64Encoder
import org.xml.sax.InputSource
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

class ArrangeTextDumpHTML[U <: File2URIBase](val arrangetree: ArrangeText,
                                             f2uri: U)(implicit cfg: CFG)
        extends ArrangeTextDump(arrangetree) {

    val path = cfg.path
    val v2f = arrangetree.tree.toMap
    val average = arrangetree.tree.average.normal
    val clusters = arrangetree.clusters.zipWithIndex.toList
    val index = arrangetree.index
    val central: File = cfg.target

    def dump() = {
        path / "index.html" write xml(central).toString
    }

    def name(file: File) = {
        val filter = Set("http", "en.wikipedia.org", "wiki")

        val name1 = file.getName().split("-").dropWhile(filter).map {
            _.replace("_", " ")
        } mkString ("")

        if (cfg.ishtml) {
            (new NoBindingFactoryAdapter).loadXML(
                new InputSource(new FileInputStream(file)),
                new SAXFactoryImpl().newSAXParser()
            ) \\ "title" headOption match {
                case Some(<title>{ title @ _* }</title>) => title
                case _                                   => ""
            }
        } else {
            file.readLines()
                .toList.map(_.trim)
                .dropWhile(_ == "")
                .headOption
                .getOrElse(name1)
        }
    }

    def xml(central: File) = {
        <html lang="en">
            <head>
                <meta charset="utf-8"/>
                <title>Clusters of Neighbours of { /* root.getName() */ }</title>
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
                    <h1> Clusters of Neighbours of <a href={ f2uri(central).toString() }>{
                        name(central)
                    } </a> </h1>
                    <p>
                        This is an outcome of an algorithm that clusters
                        { v2f.size }
                        of pages, closest to the page
                            "{ name(central) }
                        ".
                            Each of the
                        { clusters.size }
                        groups below contains a few links that 
                            point out to the pages with similar 
                            content. The words in the title describes  traits 
                            of the content, up to 100 words are printed out 
                            above an article when group is opened. Length of 
                            path through the tree equals
                        { arrangetree.tree_aligned.pathlength }
                        .
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
                                    case (v, j) => (
                                        for {
                                            file <- v2f.get(v)
                                            uri <- f2uri.get(file)
                                        } yield {
                                            <li><nobr> {
                                                val isframe = file.getName().split("-") match {
                                                    case Array(x, y, "Special", _@ _*)=> false
                                                    case _ => true
                                                }

                                                if (isframe) <a target="wiki" href={ uri.toString }> {
                                                    name(file)
                                                } </a>
                                                else <a target="_blank" href={ uri.toString } color="gray"> {
                                                    name(file)
                                                } </a>
                                            } </nobr>
                                            </li>

                                        }).getOrElse("")
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
                                        {
                                            vs.reduce(_ + _).self.toList.sortBy(-_._2).takeWhile(_._2 > 0).take(100).map(x => {
                                                index.rmap(x._1)
                                            }).sorted.mkString(" ")
                                        }
                                    </div>
                            }
                        }
                    </div>
                    <br/>
                    <iframe src={ f2uri(central).toString() } height="100%" width="100%" name="wiki"></iframe>
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
}

/*
 * Three different way resolve filename into links:
 *  
 *   - content of file is converted into URI as base64
 *   - a dictionary maps name of file into URI
 *   - parse file name with respect to how it was produced
 */

abstract class File2URIBase()(implicit cfg: CFG) {
    def apply(f: File): URI = get(f).head
    def get(f: File): Option[URI]

}

class File2URITransform()(implicit cfg: CFG) extends File2URIBase {
    def get(f: File): Option[URI] =
        f.getName().split("-") match {
            case Array(x, y, z)       => Try(new URI("http://" + x + "/" + y + "/" + z)).toOption
            case Array(x, y, z1, z2)  => Try(new URI("http://" + x + "/" + y + "/" + z1 + ":" + z2)).toOption
            case Array(x, y, ls @ _*) => Try(new URI("http://" + x + "/" + y + "/" + ls.dropRight(1).mkString(":") + "/" + ls.lastOption.getOrElse(""))).toOption
            case _                    => None
        }
}

class File2URIMap()(implicit cfg: CFG) extends File2URIBase {
    val map: Map[String, URI] = cfg.map.readLines.map {
        case x => x.split(" : ")
    } map {
        case Array(f, uri) => f -> new URI(uri)
    } toMap

    def get(f: File): Option[URI] = map.get(f.getName())
}

class File2URIDump()(implicit cfg: CFG) extends File2URIBase {
    def get(f: File): Option[URI] = Try(
        new URI({
            val text = f.readLines().mkString("\n")
            "data:text/html;base64," + new BASE64Encoder().encode((
                if (cfg.ishtml)
                    text
                else
                    <pre> { text } </pre>
            ).toString.getBytes()

            ).split("\n").mkString("")
        })
    ) match {
            case fail @ Failure(x) =>
                log("Read %s error: %s", f, x)
                None
            case Success(uri) => Option(uri)
        }
}
