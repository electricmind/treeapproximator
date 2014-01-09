package test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ru.wordmetrix.webcrawler._
import java.net.URL
import java.net.URI

class testGather extends FlatSpec with Matchers {
    val xml = <html><head><title>It's about a test</title></head><body>
                                                                     <h1><a href="http://example.org/test" shape="rect">Test of Gather Class</a></h1>
                                                                     <p><a href="http://example.org" shape="rect">Gather!</a></p>
                                                                 </body></html>

    implicit val cfg = CFG()
    val gather = new Gather(null, null, null)

    "A gather" should "parse xml" in {
        //TODO: test requires _whole - why?
        gather.page2xml_whole(xml.toString) should be(xml)
    }

    "A gather" should "elicit a set of links" in {
//        gather.xml2seeds(gather.page2xml(xml.toString)) should be(Set("http://example.org/test", "http://example.org").map(x => new URI(x)))
    }

    /* TODO: turn text back "A gather" should "compute a vector" in {
        gather.xml2vector(gather.page2xml_whole(xml.toString)) should be(List(
            ("Class", 1.0), ("Gather", 1.0), ("Gather!", 1.0), ("It's", 1.0),
            ("Test", 1.0), ("a", 1.0), ("about", 1.0), ("of", 1.0),
            ("test", 1.0)))
    }*/
}