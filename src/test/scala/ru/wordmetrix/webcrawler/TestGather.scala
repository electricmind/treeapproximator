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
import EvaluatePriorityMatrix._

class TestGather extends TestKit(ActorSystem("TestKitUsageSpec"))
        with Tools
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import Gather._
    val cfg = CFG()

    def linkcontext(n: Int) = GatherLinkContext(
        uri(n),
        Map(uri(n) ->
            Vector(
                new FeatureName("a") -> 1.0,
                new FeatureName("body") -> 1.0
            ),
            uri(n + 1) -> Vector(
                new FeatureName("a") -> 1.0,
                new FeatureName("body") -> 1.0
            ),
            uri(n + 2) -> Vector(
                new FeatureName("a") -> 1.0,
                new FeatureName("body") -> 1.0)
        )
    )

    "A gather" should {

        "parse a page" in {
            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = testParent(
                Gather.props(cfg),
                testActor,
                "Gather_1")

            queue.send(gather, GatherLink(storage.ref, sample.ref))
            queue.send(gather, GatherPage(uri(1), xml(1).toString))

            within(400 milliseconds) {
                // Return links
                expectMsg(GatherSeeds(
                    uri(1),
                    Set(uri(1), uri(2), uri(3)),
                    Vector("test" -> 15.0))
                )

                // Return text
                storage.expectMsg(GatherIntel(uri(1), text(1)))

                // Return link context
                sample.expectMsg(linkcontext(1))
            }
        }

        "propagate stoptargeting" in {
            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = testParent(
                Gather.props(cfg),
                testActor,
                "Gather_1")

            queue.send(gather, GatherLink(storage.ref, sample.ref))
            queue.send(gather, GatherPage(uri(1), xml(1).toString))
            queue.send(gather, EvaluatePriorityMatrixStopTargeting)

                expectMsg(GatherSeeds(
                    uri(1),
                    Set(uri(1), uri(2), uri(3)),
                    Vector("test" -> 15.0))
                )

                // Return text
                storage.expectMsg(GatherIntel(uri(1), text(1)))

                // Return link context
                sample.expectMsg(linkcontext(1))
                
                expectMsg(EvaluatePriorityMatrixStopTargeting)

        }

        "elicit a LinkContext from a complicate page" in {
            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = testParent(
                Gather.props(cfg),
                testActor,
                "Gather_2")

            val xml = <html><head><title>It's about a test</title></head><body>
                                                                             <h1><a href={ uri(1).toString } shape="rect">Test of Gather Class</a></h1>
                                                                             <p><a href={ uri(2).toString } shape="rect">Gather!</a></p>
                                                                         </body></html>

            queue.send(gather, GatherLink(storage.ref, sample.ref))
            queue.send(gather, GatherPage(uri(1), xml.toString))

            // Return link context
            sample.expectMsg(GatherLinkContext(
                uri(1),
                Map(uri(1) ->
                    Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0,
                        new FeatureName("h1") -> 1.0
                    ),
                    uri(2) -> Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0,
                        new FeatureName("p") -> 1.0
                    )
                )
            ))

            storage.expectMsgClass(classOf[GatherIntel[String]])
            expectMsgClass(classOf[GatherSeeds])
        }

        "remove links if repeat" in {
            val queue = TestProbe()
            val storage = TestProbe()
            val sample = TestProbe()

            val gather = testParent(
                Gather.props(cfg), testActor,
                "Gather_3")

            queue.send(gather, GatherLink(storage.ref, sample.ref))
            queue.send(gather, GatherPage(uri(1), xml(1).toString))

            expectMsg(GatherSeeds(
                uri(1),
                Set(uri(1), uri(2), uri(3)),
                Vector("test" -> 15.0))
            )
            storage.expectMsg(GatherIntel(uri(1), text(1)))

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

            storage.expectMsg(GatherIntel(uri(2), text(1)))

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
                Gather.props(cfg), testActor,
                "Gather_4")

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

            expectMsg(GatherSeeds(
                uri(1),
                Set(uri(1)),
                Vector())
            )
            storage.expectMsgClass(classOf[GatherIntel[String]])

            sample.expectMsg(GatherLinkContext(
                uri(1),
                Map(uri(1) ->
                    Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0
                    )
                )
            ))
        }
    }
}


