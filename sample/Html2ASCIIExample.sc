import ru.wordmetrix.utils.Html2Ascii
import scala.xml.Text

object Html2ASCIIExample {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def uri(n : Int ) = ""                          //> uri: (n: Int)String
  val n = 1                                       //> n  : Int = 1
  val xml = <html><head><title>{ Text( s"Text${n}" )} </title><p>asdas</p></head><body>
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
                            </body></html>        //> xml  : scala.xml.Elem = <html><head><title>Text1 </title><p>asdas</p></head>
                                                  //| <body>
                                                  //| <title>sdasdas</title>
                                                  //|                                 <a href="">
                                                  //|                                     Test Test Test Test Test
                                                  //|                                 </a>
                                                  //|                                 <a href="">
                                                  //|                                     Test Test Test Test Test
                                                  //|                                 </a>
                                                  //|                                 <a href="">
                                                  //|                                     Test Test Test Test Test
                                                  //|                                 </a>
                                                  //|                             </body></html>
                            
                            
  xml \\ "title"                                  //> res0: scala.xml.NodeSeq = NodeSeq(<title>Text1 </title>, <title>sdasdas</tit
                                                  //| le>)
                            
   val ha = Html2Ascii(xml)                       //> ha  : ru.wordmetrix.utils.Html2Ascii = ru.wordmetrix.utils.Html2Ascii@2b20bf
                                                  //| 2c
   
   ha.wrap(72)                                    //> (:::,<html><head><title>Text1 </title><p>asdas</p></head><body>
                                                  //| <title>sdasdas</title>
                                                  //|                                 <a href="">
                                                  //|                                     Test Test Test Test Test
                                                  //|                                 </a>
                                                  //|                                 <a href="">
                                                  //|                                     Test Test Test Test Test
                                                  //|                                 </a>
                                                  //|                                 <a href="">
                                                  //|                                     Test Test Test Test Test
                                                  //|                                 </a>
                                                  //|                             </body></html>)
                                                  //| res1: String = "== html==
                                                  //| == head==
                                                  //| Text1
                                                  //| ======
                                                  //| 
                                                  //| asdas
                                                  //| 
                                                  //| == body==
                                                  //| 
                                                  //| sdasdas
                                                  //| =======
                                                  //| 
                                                  //| 
                                                  //| Test Test Test Test Test
                                                  //| 
                                                  //| 
                                                  //| Test Test Test Test Test
                                                  //| 
                                                  //| 
                                                  //| Test Test Test Test Test
                                                  //| 
                                                  //| "
                            
 
}