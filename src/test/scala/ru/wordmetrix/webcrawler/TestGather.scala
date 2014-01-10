package ru.wordmetrix.webcrawler

import java.net.URI

import scala.concurrent.duration.DurationInt

import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpecLike }

import Gather.{ GatherIntel, GatherLinkContext, GatherPage, GatherSeeds }
import akka.actor.ActorSystem
import akka.testkit.{ DefaultTimeout, ImplicitSender, TestKit, TestProbe }
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.webcrawler.LinkContext.FeatureName

class TestGather extends TestKit(ActorSystem("TestKitUsageSpec"))

        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    val cfg = CFG()

    "An gather" should {
        "parse a page" in {

            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = system.actorOf(
                Gather.props(queue.ref, storage.ref, sample.ref, cfg),
                "Gather_parses_a_page")

            val uri = new URI("http://example.org")
            
            val xml = <html><body>
                                <a href="http://en.wikipedia.org/qq">
                                    Test Test Test Test Test Test
                                </a>
                            </body></html>

            queue.send(gather, GatherPage(uri, xml.toString))

            within(100 milliseconds) {
                queue.expectMsg(GatherSeeds(
                    uri,
                    Set(new URI("http://en.wikipedia.org/qq")),
                    Vector("test" -> 6.0))
                )
                storage.expectMsg(GatherIntel(uri, "== html=="))

                sample.expectMsg(GatherLinkContext(
                    uri,
                    Map(new URI("http://en.wikipedia.org/qq") ->
                        Vector(
                            new FeatureName("a") -> 1.0,
                            new FeatureName("body") -> 1.0)
                    ))
                )
            }
        }
    }
}
