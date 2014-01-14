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
import akka.actor.Props
import akka.actor.Actor

class TestGather extends TestKit(ActorSystem("TestKitUsageSpec"))
        with Tools
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import Gather._
    val cfg = CFG()
    
    "An gather" should {

        "parse a page" in {
            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = testParent(
                Gather.props(cfg),
                testActor,
                "Gather_parses_a_page")

            queue.send(gather, GatherLink(storage.ref, sample.ref))
            queue.send(gather, GatherPage(uri(1), xml(1).toString))

            within(400 milliseconds) {
                expectMsg(GatherSeeds(
                    uri(1),
                    Set(uri(1), uri(2), uri(3)),
                    Vector("test" -> 15.0))
                )
                storage.expectMsg(GatherIntel(uri(1), "== html=="))

                sample.expectMsg(GatherLinkContext(
                    uri(1),
                    Map(uri(1) ->
                        Vector(
                            new FeatureName("a") -> 1.0,
                            new FeatureName("body") -> 1.0
                        ),
                        uri(2) -> Vector(
                            new FeatureName("a") -> 1.0,
                            new FeatureName("body") -> 1.0
                        ),
                        uri(3) -> Vector(
                            new FeatureName("a") -> 1.0,
                            new FeatureName("body") -> 1.0)
                    )
                )
                )
            }
        }

        "remove links if repeat" in {
            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = testParent(
                Gather.props(cfg), testActor,
                "Gather_parses_a_page")

            queue.send(gather, GatherLink(storage.ref, sample.ref))
            queue.send(gather, GatherPage(uri(1), xml(1).toString))

            expectMsg(GatherSeeds(
                uri(1),
                Set(uri(1), uri(2), uri(3)),
                Vector("test" -> 15.0))
            )
            storage.expectMsg(GatherIntel(uri(1), "== html=="))

            sample.expectMsg(GatherLinkContext(
                uri(1),
                Map(uri(1) ->
                    Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0
                    ),
                    uri(2) -> Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0
                    ),
                    uri(3) -> Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0)
                )
            ))

            queue.send(gather, GatherPage(uri(2), xml(1).toString))

            expectMsg(GatherSeeds(
                uri(2),
                Set(),
                Vector("test" -> 15.0))
            )

            storage.expectMsg(GatherIntel(uri(2), "== html=="))

            sample.expectMsg(GatherLinkContext(
                uri(2),
                Map(uri(1) ->
                    Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0
                    ),
                    uri(2) -> Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0
                    ),
                    uri(3) -> Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0)
                )
            ))

        }

        "remove links if different from parent" in {
            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = testParent(
                Gather.props(cfg),testActor,
                "Gather_parses_a_page")

            queue.send(gather, GatherLink(storage.ref, sample.ref))
            queue.send(gather, GatherPage(uri(1),
                <html>
                    <body>
                        <a href="http://test.example.org"/>
                        <a href="http://example.org/1"/>
                    </body>
                </html>.
                    toString
            ))

            within(400 milliseconds) {
                expectMsg(GatherSeeds(
                    uri(1),
                    Set(uri(1)),
                    Vector())
                )
                storage.expectMsg(GatherIntel(uri(1), "== html=="))

                sample.expectMsg(GatherLinkContext(
                    uri(1),
                    Map(uri(1) ->
                        Vector(
                            new FeatureName("a") -> 1.0,
                            new FeatureName("body") -> 1.0
                        )
                    )
                )
                )
            }
        }

    }

    "A gather method" should {
        "parse xml" in {
            //gather.page2xml_whole(xml.toString) should be(xml)
        }
    }
}
