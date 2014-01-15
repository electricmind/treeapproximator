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

class TestEvaluatePriorityMatrix extends TestKit(ActorSystem("TestEvalutatePriorityMatrix"))
with Tools
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import EvaluatePriorityMatrix._
    import SeedQueue._
    import Gather._
    import Storage._
    import SampleHierarchy2Priority._

    val cfg = CFG(isdebug=true, targets=2, targeting = 0.005)
   
    "A queue" should {
        "init a process" in {
            val (storage, storageprop) = TestActor()
            val (gather, gatherprop) = TestActor()
            val (seedqueue, seedqueueprop) = TestActor()
            val (sample, sampleprop) = TestActor()

            val queue = system.actorOf(
                EvaluatePriorityMatrix.props(storageprop, gatherprop, seedqueueprop, sampleprop, cfg),
                "TestEvaluatePriority_1")

            def checkuri(n: Int): PartialFunction[Any, Unit] = {
                val u = uri(n)

                {
                    case (SeedQueueRequest(`u`)) =>
                }
            }

            // Initial seed is sent     
            queue ! EvaluatePriorityMatrixSeed(uri(1))
            
            storage.expectMsgClass(classOf[StorageVictim])

            gather.expectMsgClass(classOf[GatherLink])
            
            // Initial phase
            seedqueue.expectMsgClass(classOf[SeedQueueLink])
            
            seedqueue.expectMsgPF()(checkuri(1))

            // Targeting phase
            gather.send(queue, GatherSeeds(uri(1), Set(uri(2), uri(3), uri(4), uri(5), uri(6), uri(7)), Vector("test" -> 2.0)))

            seedqueue.expectMsgPF()(checkuri(2))

            seedqueue.expectMsgPF()(checkuri(3))

            seedqueue.expectMsgPF()(checkuri(4))

            seedqueue.expectMsgPF()(checkuri(5))

            seedqueue.expectMsgPF()(checkuri(6))

            seedqueue.expectMsgPF()(checkuri(7))

            storage.expectMsg(StorageSign(uri(1)))

            gather.send(queue, GatherSeeds(uri(2), Set(uri(4), uri(5)), Vector("test" -> 2.0, "test2" -> 4.0)))

            storage.expectMsg(StorageSign(uri(2)))

            gather.send(queue, GatherSeeds(uri(3), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test3" -> 3.0)))

            storage.expectMsg(StorageSign(uri(3)))

            gather.send(queue, GatherSeeds(uri(4), Set(uri(4), uri(5)), Vector("test" -> 2.0, "test4" -> 2.0)))

            gather.send(queue, GatherSeeds(uri(5), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test5" -> 1.0)))

            storage.expectMsg(StorageSign(uri(5)))
  
            // Estimation phase
            gather.send(queue, GatherSeeds(uri(6), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test6" -> 0.5)))

            gather.send(queue, GatherSeeds(uri(7), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test7" -> 0.25)))

            seedqueue.expectMsg(SeedQueueAvailable)
            //sample.expectMsg(1)
        }
        
        "stop targeting phase if failed" in {
            val (storage, storageprop) = TestActor()
            val (gather, gatherprop) = TestActor()
            val (seedqueue, seedqueueprop) = TestActor()
            val (sample, sampleprop) = TestActor()

            val queue = system.actorOf(
                EvaluatePriorityMatrix.props(storageprop, gatherprop, seedqueueprop, sampleprop, cfg),
                "TestEvaluatePriority_1")

            def checkuri(n: Int): PartialFunction[Any, Unit] = {
                val u = uri(n)

                {
                    case (SeedQueueRequest(`u`)) =>
                }
            }

            // Initial seed is sent     
            queue ! EvaluatePriorityMatrixSeed(uri(1))
            
            storage.expectMsgClass(classOf[StorageVictim])

            gather.expectMsgClass(classOf[GatherLink])
            
            // Initial phase
            seedqueue.expectMsgClass(classOf[SeedQueueLink])
            
            seedqueue.expectMsgPF()(checkuri(1))

            // Targeting phase
            watch(queue)
            
            gather.send(queue, GatherSeeds(uri(1), Set(uri(2), uri(3), uri(4), uri(5), uri(6), uri(7)), Vector("test" -> 2.0)))

            seedqueue.expectMsgPF()(checkuri(2))

            seedqueue.expectMsgPF()(checkuri(3))

            seedqueue.expectMsgPF()(checkuri(4))

            seedqueue.expectMsgPF()(checkuri(5))

            seedqueue.expectMsgPF()(checkuri(6))

            seedqueue.expectMsgPF()(checkuri(7))
            
            seedqueue.expectMsg(EvaluatePriorityMatrixStopTargeting)

            gather.send(queue,EvaluatePriorityMatrixStopTargeting)
            
            expectTerminated(queue)
            
        }

    }
}
