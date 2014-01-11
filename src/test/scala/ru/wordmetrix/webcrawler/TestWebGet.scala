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

class TestWebGet extends TestKit(ActorSystem("TestKitUsageSpec"))

        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import SeedQueue._
    val cfg = CFG()

    "A webget" should {
        "get a page" in {

            val seedqueue = TestProbe()
            val gather = TestProbe()

            val webget = system.actorOf(
                WebGet.props(cfg),
                "WebGetTest_1")
                
            watch(webget)
            val uri = new URI("http://example.org")
            
            val xml = <html><body>
                                <a href="http://en.wikipedia.org/qq">
                                    Test Test Test Test Test Test
                                </a>
                            </body></html>

            seedqueue.send(webget, SeedQueueRequest(uri,gather.ref))
            seedqueue.expectMsg(SeedQueueGet)
            seedqueue.send(webget, SeedQueueEmpty)
            
            gather.expectMsgPF() {
                case msg @ GatherPage(`uri`,_) => msg 
            }
            
            expectTerminated(webget)
        }
    }
}
