package ru.wordmetrix.utils
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.CharArrayReader
import scala.util.Random
import scala.Array.canBuildFrom
import scala.xml.NodeSeq.seqToNodeSeq

object Html2Ascii {
    def apply(page: scala.xml.NodeSeq) = {
        new Html2Ascii(page)
    }
}

class Html2Ascii(page: scala.xml.NodeSeq) {

    def this(page: String) = this(
        (new NoBindingFactoryAdapter).loadXML(
            new InputSource(new CharArrayReader(page.toArray)),
            new SAXFactoryImpl().newSAXParser()) //to //\\ "BODY"
    )

    val div = Set("p", "title", "h1", "h2", "h3", "h4", "h5", "h6", "div")
    val span = Set("i", "a", "b", "span", "sup")
    val discard = Set("img", "script", "form", "input", "button")

    def justify(x: String, size: Int = 72) = {
        val gap = size - x.length
        x.split(" ").filterNot(_ == "") match {
            case words if words.length <= 1 => x
            case words =>
                val q1 = gap / (words.length - 1)
                val q2 = gap % (words.length - 1)
                val spaces1 = (1 until words.length).map(x => " " * (q1 + 1))
                val spaces2 = Random.shuffle(spaces1.take(q2).map(_ + " ") ++
                    spaces1.drop(q2)) ++ List("")
                words.zip(spaces2).map({ case (x, y) => x + y }).mkString
        }
    }

    def wrap(size: Int = 72) = {
        dump(page).split("\n").map({
    
        case x if x.length < size => List(x)
        case x => {
            x.split(" ").foldLeft(List[String]()) {
                case (s :: ss, w) => s + " " + w match {
                    case s if s.length < size => s :: ss
                    case _ if w.length > size => w :: justify(s, size) :: ss
                    case _                    => w :: justify(s, size) :: ss
                }
                case (List(), w) => List(w)
            }.reverse
        }
    }).flatten.map(_.trim).mkString("\n")
    }
    def dump(nodes: Seq[scala.xml.Node] = page): String = {
        (nodes map {
            case <html>{ nodes @ _* }</html> =>
                dump(nodes)
                
            case <title>{ nodes @ _* }</title> => 
                val s = dump(nodes)
                s + "\n" + ("=" * s.length()) + "\n"
                
            case <h1>{ nodes @ _* }</h1> => "\n = " + dump(nodes) + " =\n"
            case <h2>{ nodes @ _* }</h2> => "\n == " + dump(nodes) + " ==\n"
            case <h3>{ nodes @ _* }</h3> => "\n === " + dump(nodes) + " ===\n"
            case <h4>{ nodes @ _* }</h4> => "\n ==== " + dump(nodes) + " ====\n"
            case <h5>{ nodes @ _* }</h5> => "\n ===== " + dump(nodes) + " =====\n"
            case <h6>{ nodes @ _* }</h6> => "\n ====== " + dump(nodes) + " ======\n"
           
            
            case <table>{ tr @ _* }</table> => "\n" + "=" * 60 + "\n" + (tr map {
                case <tr>{ td @ _* }</tr> => td map {
                    case <td>{ nodes @ _* }</td> => dump(nodes) match {
                        case x if x contains "\n" => " -- \n" + x + " --"
                        case x                    => x
                    }
                    case <th>{ nodes @ _* }</th> => dump(nodes) match {
                        case x if x contains "\n" => " -- \n" + x + " --"
                        case x                    => x
                    }
                    case nodes => dump(nodes)
                } mkString (" | ")
                case nodes => dump(nodes)
            } mkString ("\n" + "-" * 60 + "\n")) + "\n" + "=" * 60 + "\n"

            case <br/> => "\n"

            case <ul>{ li @ _* }</ul> => li map {
                case <li>{ nodes @ _* }</li> => "\n - " + dump(nodes) + "\n"
                case nodes                   => dump(nodes)
            } mkString

            case <ol>{ li @ _* }</ol> => li.zipWithIndex.map {
                case (<li>{ nodes @ _* }</li>, i) => "\n " + i + " " + dump(nodes) + "\n"
                case (node, i)                    => dump(node)
            } mkString

            case xml.Text(t) => t

            case xml.Elem(_, label, _, _, nodes @ _*) if div contains label =>
                "\n" + dump(nodes) + "\n"

            case xml.Elem(_, label, _, _, nodes @ _*) if span contains label =>
                dump(nodes)

            case xml.Elem(
                _, label, _, _, nodes @ _*
                ) if discard contains label => ""

            case xml.Elem(_, label, _, _, nodes @ _*) =>
                "== " + label + "==\n" + dump(nodes) + "\n"

            case x => 
                 println("Html2Ascii unknown: %s %s", x.label, x); 
                 ""
        }) mkString
    }

}