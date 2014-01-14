/**
 *
 */
package ru.wordmetrix.webcrawler

import org.scalatest.Matchers
import java.net._
import ru.wordmetrix.webcrawler.WebCrawler._
import org.scalatest.FlatSpec
import org.scalatest.WordSpecLike
/**
 * @author cray
 *
 */
class TestWebCrawler extends WordSpecLike with Matchers {

    "A normalilze" should {
        "create an absolute uri" in {

            normalize(new URI("http://example.org/"), "example") should
                be(new URI("http://example.org/example"))
            normalize("http://example.org/", "example") should
                be(new URI("http://example.org/example"))
        }

        "create an uri" in {
            normalize("http://example.org/example") should
                be(new URI("http://example.org/example"))
        }

        "split the fragment" in {
            normalize(new URI("http://example.org/example#1")) should
                be(new URI("http://example.org/example"))
        }
        "remove .. and ." in {
            normalize(new URI("http://example.org/q/../example")) should
                be(new URI("http://example.org/example"))
        }
    }

}