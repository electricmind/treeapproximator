/**
 *
 */
package test

import org.scalatest.Matchers
import java.net._
import ru.wordmetrix.webcrawler.WebCrawler._
import org.scalatest.FlatSpec
/**
 * @author cray
 *
 */
class testWebCrawler extends FlatSpec with Matchers {
    "A normalilze" should "should create an absolute uri" in {
        normalize(new URI("http://example.org/"),"example") should
            be(new URI("http://example.org/example"))
        normalize("http://example.org/","example") should
            be(new URI("http://example.org/example"))
    }
    
    "A normalilze" should "should create an uri" in {
        normalize("http://example.org/example") should
            be(new URI("http://example.org/example"))
    }

    "A normalilze" should "split the fragment" in {
        normalize(new URI("http://example.org/example#1")) should
            be(new URI("http://example.org/example"))
    }
    "A normalilze" should "remove .. and ." in {
        normalize(new URI("http://example.org/q/../example")) should
            be(new URI("http://example.org/example"))
    }

}