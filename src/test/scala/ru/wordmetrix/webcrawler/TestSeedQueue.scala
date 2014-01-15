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

class TestSeedQueue extends TestKit(ActorSystem("TestSeedQueue"))
        with Tools
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import SeedQueue._
    import WebGet._

    val cfg = CFG()

    "A single-item seedqueue" should {
        val cfg = CFG(servers = 1)
        "run one request" in {

            val queue = TestProbe()
            val gather = TestProbe()

            val (webget, webgetprop) = TestActor()

            val seedqueue = testParent(
                SeedQueue.props(webgetprop, cfg), testActor,
                "TestSeedQueue1_1")

            val uri = new URI("http://example.org")

            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri))

            webget.expectMsg(WebGetRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)
        }

        "answer announce that seeds are available" in {

            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()

            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = testParent(
                SeedQueue.props(webgetprop, cfg), testActor, "TestSeedQueue1_2")

            val uri = new URI("http://example.org")
            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri))

            webget.expectMsg(WebGetRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)

            queue.send(seedqueue, SeedQueueAvailable)

            queue.expectMsg(SeedQueueGet)
        }

        "queued a request" in {

            val queue = TestProbe()
            val gather = TestProbe()

            val (webget, webgetprop) = TestActor()

            val seedqueue = //system.actorOf(
                testParent(SeedQueue.props(webgetprop, CFG(servers = 1)),
                    testActor, "TestSeedQueue1_3")

            val uri = new URI("http://example.org")
            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri))

            queue.send(seedqueue, SeedQueueRequest(uri))

            webget.expectMsg(WebGetRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(WebGetRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)
        }

        "run two requests" in {

            val queue = TestProbe()
            //            val webget = TestProbe()
            val gather = TestProbe()

            val (webget, webgetprop) = TestActor()

            val seedqueue = testParent(
                SeedQueue.props(webgetprop, cfg), testActor, "TestSeedQueue1_4")

            val uri = new URI("http://example.org")
            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri))

            webget.expectMsg(WebGetRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)

            queue.send(seedqueue, SeedQueueRequest(uri))

            webget.expectMsg(WebGetRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)
        }

    }

    "A two-items seedqueue" should {
        val cfg = CFG(servers = 2)

        def uri(n: Int = 0): URI = new URI(s"http://example.org/${n}")

        "run two request" in {

            val queue = TestProbe()
            val gather = TestProbe()

            val (webget, webgetprop) = TestActor()

            val seedqueue = testParent(
                SeedQueue.props(webgetprop, cfg), testActor, "TestSeedQueue2_1")

            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(1)))

            queue.send(seedqueue, SeedQueueRequest(uri(2)))

            //TODO: Strange behavior here, sometimes requests come miss ordered

            webget.expectSet(
                    WebGetRequest(uri(1), gather.ref),
                    WebGetRequest(uri(2), gather.ref)
            )

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)
        }

        "queued a request" in {

            val queue = TestProbe()
            val gather = TestProbe()

            val (webget, webgetprop) = TestActor()

            val seedqueue = testParent(
                SeedQueue.props(webgetprop, CFG(servers = 1)),
                testActor, "TestSeedQueue2_2")
            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(1)))

            queue.send(seedqueue, SeedQueueRequest(uri(2)))

            queue.send(seedqueue, SeedQueueRequest(uri(3)))

            webget.expectMsg(WebGetRequest(uri(1), gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(WebGetRequest(uri(2), gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(WebGetRequest(uri(3), gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)
        }

        "run four requests" in {
            val queue = TestProbe()
            val gather = TestProbe()

            val (webget, webgetprop) = TestActor()

            val seedqueue = testParent(
                SeedQueue.props(webgetprop, cfg), testActor, "TestSeedQueue2_3")

            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(1)))

            queue.send(seedqueue, SeedQueueRequest(uri(2)))

            webget.expectSet(
                    WebGetRequest(uri(1), gather.ref),
                    WebGetRequest(uri(2), gather.ref)
            )

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)

            // second pass

            queue.send(seedqueue, SeedQueueRequest(uri(3)))

            queue.send(seedqueue, SeedQueueRequest(uri(4)))

            webget.expectSet(
                    WebGetRequest(uri(3), gather.ref),
                    WebGetRequest(uri(4), gather.ref)
            )

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)
        }

        "propagate targeting" in {

            val queue = TestProbe()
            val gather = TestProbe()

            val (webget, webgetprop) = TestActor()

            val seedqueue = testParent(
                SeedQueue.props(webgetprop, cfg), testActor, "TestSeedQueue2_4")

            queue.send(seedqueue, SeedQueueLink(gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(1)))

            queue.send(seedqueue, SeedQueueRequest(uri(2)))

            queue.send(seedqueue, EvaluatePriorityMatrixStopTargeting)
            
            webget.expectSet(
                    WebGetRequest(uri(1), gather.ref),
                    WebGetRequest(uri(2), gather.ref)
            )

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            expectMsg(SeedQueueGet)

            gather.expectMsg(EvaluatePriorityMatrixStopTargeting)
        }

    }
}
