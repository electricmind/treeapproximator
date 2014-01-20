import ru.wordmetrix.utils.Html2Ascii
import scala.xml.Text
import scala.xml.parsing.NoBindingFactoryAdapter
import org.xml.sax.InputSource
//import scala.util.parsing.input.CharArrayReader
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.CharArrayReader
import scala.util.Random
import scala.Array.canBuildFrom
import scala.xml.NodeSeq.seqToNodeSeq

object Html2ASCIIExample {
    println("Welcome to the Scala worksheet")     //> Welcome to the Scala worksheet

    def uri(n: Int) = ""                          //> uri: (n: Int)String
    val n = 1                                     //> n  : Int = 1
    val xml = <html><head><title>{ Text(s"Text${n}") } </title><p>asdas</p></head><body>
                                                                                      <title>sdasdas</title>
                                                                                      <a href={ uri(n).toString() }>
                                                                                          Test Test Test Test Test
                                                                                      </a>
                                                                                      <a href={ uri(n + 1).toString() }>
                                                                                          Test Test Test Test Test
                                                                                      </a>
                                                                                      <a href={ uri(n + 2).toString() }>
                                                                                          Test Test Test Test Test
                                                                                      </a>
                                                                                  </body></html>
                                                  //> xml  : scala.xml.Elem = <html><head><title>Text1 </title><p>asdas</p></head
                                                  //| ><body>
                                                  //|                                                                            
                                                  //|            <title>sdasdas</title>
                                                  //|                                                                            
                                                  //|            <a href="">
                                                  //|                                                                            
                                                  //|                Test Test Test Test Test
                                                  //|                                                                            
                                                  //|            </a>
                                                  //|                                                                            
                                                  //|            <a href="">
                                                  //|                                                                            
                                                  //|                Test Test Test Test Test
                                                  //|                                                                            
                                                  //|            </a>
                                                  //|                                                                            
                                                  //|            <a href="">

    xml \\ "title"                                //> res0: scala.xml.NodeSeq = NodeSeq(<title>Text1 </title>, <title>sdasdas</ti
                                                  //| tle>)

    val ha = Html2Ascii(xml)                      //> ha  : ru.wordmetrix.utils.Html2Ascii = ru.wordmetrix.utils.Html2Ascii@2b20b
                                                  //| f2c

    ha.wrap(72)                                   //> res1: String = Text1
                                                  //| ======
                                                  //| 
                                                  //| asdas
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| sdasdas
                                                  //| =======
                                                  //| 
                                                  //| 
                                                  //| Test Test Test Test Test
                                                  //| 
                                                  //| Test Test Test Test Test
                                                  //| 
                                                  //| Test Test Test Test Test

    new Html2Ascii("ssss").wrap(100)              //> res2: String = ssss

    (new NoBindingFactoryAdapter).loadXML(
        new InputSource(new CharArrayReader("<html><title>adasd</title></html>".toCharArray())),
        new SAXFactoryImpl().newSAXParser()) \\ "title" headOption match {
            case Some(<title>{ title @ _* }</title>) => title
            case _ => ""
        }                                         //> res3: Object = List(adasd)

}