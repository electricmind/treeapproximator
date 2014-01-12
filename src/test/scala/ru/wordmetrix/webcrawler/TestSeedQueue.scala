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

class TestSeedQueue extends TestKit(ActorSystem("TestSeedQueue"))
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import SeedQueue._

    val cfg = CFG()

    "A single-item seedqueue" should {
        val cfg = CFG(List("-n", "1"))
        "run one request" in {

            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()

            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = system.actorOf(
                SeedQueue.props(webgetprop, cfg),
                "TestSeedQueue1")

            val uri = new URI("http://example.org")

            queue.send(seedqueue, SeedQueueRequest(uri, gather.ref))

            webget.expectMsg(SeedQueueRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)
        }

        "answer announce that seeds are available" in {

            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()

            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = system.actorOf(
                SeedQueue.props(webgetprop, cfg),
                "TestSeedQueue1")

            val uri = new URI("http://example.org")

            queue.send(seedqueue, SeedQueueRequest(uri, gather.ref))

            webget.expectMsg(SeedQueueRequest(uri, gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)
            
            queue.send(seedqueue,SeedQueueAvailable)

            queue.expectMsg(SeedQueueGet)
        }

        
        "queued a request" in {

            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()

            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = system.actorOf(
                SeedQueue.props(webgetprop, CFG(List("-n", "1"))),
                "TestSeedQueue2")

            val uri = new URI("http://example.org")

            queue.send(seedqueue, SeedQueueRequest(uri,gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri,gather.ref))

            webget.expectMsg(SeedQueueRequest(uri,gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueRequest(uri,gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)
        }

        "run two requests" in {

            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()


            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = system.actorOf(
                SeedQueue.props(webgetprop, cfg),
                "TestSeedQueue3")

            val uri = new URI("http://example.org")

            queue.send(seedqueue, SeedQueueRequest(uri,gather.ref))

            webget.expectMsg(SeedQueueRequest(uri,gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)

            queue.send(seedqueue, SeedQueueRequest(uri,gather.ref))

            webget.expectMsg(SeedQueueRequest(uri,gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)
        }

    }

    "A two-items seedqueue" should {
        val cfg = CFG(List("-n", "2"))

        def uri(n: Int = 0): URI = new URI(s"http://example.org/${n}")

        "run two request" in {

            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()

            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = system.actorOf(
                SeedQueue.props(webgetprop, cfg),
                "TestSeedQueue2_1")

            queue.send(seedqueue, SeedQueueRequest(uri(1),gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(2),gather.ref))

            //TODO: Strange behavior here, sometimes requests come miss ordered

            webget.expectMsg(SeedQueueRequest(uri(1),gather.ref))
            
            webget.expectMsg(SeedQueueRequest(uri(2),gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)
        }

        "queued a request" in {

            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()

            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = system.actorOf(
                SeedQueue.props(webgetprop, CFG(List("-n", "1"))),
                "TestSeedQueue2_2")

            queue.send(seedqueue, SeedQueueRequest(uri(1),gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(2),gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(3),gather.ref))

            webget.expectMsg(SeedQueueRequest(uri(1),gather.ref))

            webget.expectMsg(SeedQueueRequest(uri(2),gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueRequest(uri(3),gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)
        }

        "run four requests" in {
            val queue = TestProbe()
            val webget = TestProbe()
            val gather = TestProbe()

            val webgetprop = Props(new Actor {
                def receive = { case msg => webget.ref forward msg }
            })

            val seedqueue = system.actorOf(
                SeedQueue.props(webgetprop, cfg),
                "TestSeedQueue2_3")

            queue.send(seedqueue, SeedQueueRequest(uri(1),gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(2),gather.ref))

            webget.expectMsg(SeedQueueRequest(uri(1),gather.ref))

            webget.expectMsg(SeedQueueRequest(uri(2),gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)

            // second pass

            queue.send(seedqueue, SeedQueueRequest(uri(3),gather.ref))

            queue.send(seedqueue, SeedQueueRequest(uri(4),gather.ref))

            webget.expectMsg(SeedQueueRequest(uri(3),gather.ref))

            webget.expectMsg(SeedQueueRequest(uri(4),gather.ref))

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)

            webget.send(seedqueue, SeedQueueGet)

            webget.expectMsg(SeedQueueEmpty)

            queue.expectMsg(SeedQueueGet)
        }
    }
}
