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
import SampleHierarchy2Priority._

class TestSampleHierarchy2Priority extends TestKit(ActorSystem("TestSampleHierarchy2Priority"))
with Tools
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    implicit val accuracy = 0.0001d

    override def afterAll(): Unit = {
        system.terminate()
    }

    val cfg = CFG()

    "An sample" should {
        "parse a page" in {

            val queue = TestProbe()
            val gather = TestProbe()
            val sample = system.actorOf(
                SampleHierarchy2Priority.props(cfg),
                "TestSample")

            gather.send(sample, GatherLinkContext(
                uri(),
                Map(new URI("http://en.wikipedia.org/qq") ->
                    Vector(
                        new FeatureName("a") -> 1.0,
                        new FeatureName("body") -> 1.0)
                )))

            queue.send(sample, SampleHirarchy2PriorityPriority(uri(), 1.0))
            // TODO: check file
        }
    }
}
