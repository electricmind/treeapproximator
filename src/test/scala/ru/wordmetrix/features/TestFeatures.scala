package ru.wordmetrix.features

import java.net.URI
import org.scalatest.{ FlatSpec, Matchers, WordSpecLike }
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.utils.impl.{ StirngEx, URIEx }
import ru.wordmetrix.utils.CFG

class TestFeatures extends WordSpecLike with Matchers {
    implicit val cfg = CFG(wordlen = 0, wordfreq = 0)

    import Features._

    val words = List("He", "eagery", "advance", "to", "more", "sophisticated",
        "experiments",
        "with", "his", "drugs", "especially", "a", "trial", "on", "human")

    val wordset = words.map(_.toLowerCase()).toSet

    val sv = Vector[String](
        wordset.map(_ -> 1.0).toList
    )

    val ids = (1 to words.size).toSet
    val iv = Vector[Int](
        ids.map(_ -> 1.0).toList
    )

    val text = words.zipWithIndex.map({
        case (w, n) => " " * n + w
    }).mkString(" ")

    "A split" should {
        "split a text" in {
            Features.split(text).toSet should be(sv.toSet)
        }

        "count words more long than 1" in {
            Features.split(text)(cfg.copy(wordlen = 1))
                .toSet should be(sv.toSet.filter(x => x._1.length > 1))
        }
        
        "split a text more often than 1" in {
            Features.split(text)(cfg.copy(wordfreq = 1))
                .toSet should be(Set())
            
            Features.split(s"$text, $text")(cfg.copy(wordfreq = 1))
                .toSet should be(sv + sv toSet)
        }

    }

    "A string vector" should {
        "be counted once" in {
            Features.fromText(text) should be(sv)
        }

        "be counted twice" in {
            Features.fromText(text + ", " + text) should be(sv + sv)
        }

        "be counted three time" in {
            Features.fromText(s"$text, $text : $text!!") should be(sv + sv + sv)
        }
    }

    "An int vector" should {
        "be counted once" in {
            Features.fromText(
                text, Features.String2Word[String, Double]()
            )._1 should be(iv)
        }

        "be counted twice" in {
            Features.fromText(
                text + ", " + text, Features.String2Word[String, Double]()
            )._1 should be(iv + iv)
        }

        "be counted three time" in {
            Features.fromText(
                s"$text, $text : $text!!", Features.String2Word[String, Double]()
            )._1 should be(iv + iv + iv)
        }
    }

    "An index" should {
        "be valid after short text" in {
            val (_, Features.String2Word(map, rmap, n)) = Features.fromText(
                text, Features.String2Word[String, Double]()
            )

            map.map(_.swap) should be(rmap)
            map.keys.toSet should be(wordset)
            rmap.keys.toSet should be(ids)
            n should be(wordset.size)
        }

        "be valid after doubled text" in {
            val (_, Features.String2Word(map, rmap, n)) = Features.fromText(
                text + ", " + text, Features.String2Word[String, Double]()
            )

            map.map(_.swap) should be(rmap)
            map.keys.toSet should be(wordset)
            rmap.keys.toSet should be(ids)
            n should be(wordset.size)
        }

    }
}