package ru.wordmetrix.webcrawler

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ru.wordmetrix.webcrawler._
import LinkContext._
import java.net.URI
import org.scalatest.WordSpecLike

class TestLinkContext extends WordSpecLike with Matchers {

    val extract = new LinkContext(new URI(""))
    "An simple xml" should {
        "gives a map with empty vectors" in {

            extract.extract(
                <html>
                    <head>
                        <title>Title</title>
                    </head>
                    <body>
                        <h1><a href="http://example.org">Header</a></h1>
                        <p><a href="http://example.org/1">SomeLink</a></p>
                        <p></p>
                    </body>
                </html>
            ) should be equals (Map("http://example.org" -> Vector[Feature](), "http://example.org/1" -> Vector[Feature]()))
        }

        "gives a map with class and id" in {
            extract.extract(
                <html>
                    <head>
                        <title>Title</title>
                    </head>
                    <body class="test">
                        <h1><a href="http://example.org">Header</a></h1>
                        <p><a id="ref" href="http://example.org/1">SomeLink</a></p>
                        <p></p>
                    </body>
                </html>
            ) should be equals (Map(
                    "http://example.org" -> Vector(
                        Feature("""class=test""") -> 1.0),
                    "http://example.org/1" -> Vector(
                        Feature("""class=test""") -> 1.0,
                        Feature("""id=ref""") -> 1.0
                    )
                ))
        }

        "gives a map with two classes" in {
            //println(Feature("""class=body""").equals(Feature("""class=body""")))
            //Feature("""class=body""") should be equals (Feature("""class=body1"""))
            
            extract.extract(
                <html>
                    <head>
                        <title>Title</title>
                    </head>
                    <body class="body">
                        <h1><a class="header" href="http://example.org">Header</a></h1>
                        <p><a id="ref" href="http://example.org/1">SomeLink</a></p>
                        <p></p>
                    </body>
                </html>
            ) should be equals (Map(
                    new URI("http://example.org") -> Vector(
                        (Feature("a"), 1.0), (Feature("body"), 1.0), (Feature("h1"), 1.0), (Feature("""class=body"""), 1.0),
                        (Feature("""class=header"""), 1.0)),

                    new URI("http://example.org/1") -> Vector((Feature("a"), 1.0), (Feature("body"), 1.0), (Feature("p"), 1.0), (Feature("""id=ref"""), 1.0),
                        (Feature("""class=body"""), 1.0))
                ))
        }

        "gives a map with one class twice" in {
            extract.extract(
                <html>
                    <head>
                        <title>Title</title>
                    </head>
                    <body class="body">
                        <h1><a class="body" href="http://example.org">Header</a></h1>
                        <p><a id="ref" href="http://example.org/1">SomeLink</a></p>
                        <p></p>
                    </body>
                </html>
            ) should be equals (Map(
                    new URI("http://example.org") -> Vector(
                        Feature("""class=body""") -> 2.0),
                    new URI("http://example.org/1") -> Vector(
                        Feature("""class=body""") -> 1.0,
                        Feature("""id=ref""") -> 1.0
                    )
                ))
        }
    }

}