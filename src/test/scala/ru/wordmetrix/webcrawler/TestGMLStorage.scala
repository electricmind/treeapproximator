package ru.wordmetrix.webcrawler

import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpecLike }
import GMLStorage.{ GMLStorageEstimator, GMLStorageSeed }
import akka.actor.ActorSystem
import akka.testkit.{ DefaultTimeout, ImplicitSender, TestKit, TestProbe }
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.vector.Vector
import java.io.File
import ru.wordmetrix.smartfile.SmartFile.fromFile
import EvaluatePriorityMatrix._

class TestNetworkDump extends TestKit(ActorSystem("TestStorage"))
        with Tools
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {
    implicit val accuracy = 0.0001d

    override def afterAll(): Unit = {
        system.terminate()
    }

    import GMLStorage._

    implicit val cfg = CFG(path = new File("/tmp/test"))

    val net1 = (new File(".") / "data" / "network1.gml").readLines.mkString
    val net2 = (new File(".") / "data" / "network2.gml").readLines.mkString

    "A networkstorage" should {
        "dump two networks" in {

            val gather = TestProbe()
            val queue = TestProbe()
            val sample = TestProbe()
            val seedqueue = TestProbe()

            val storage = system.actorOf(GMLStorage.props(cfg), "TestGMLStorage")

            (cfg.path / "network.gml").write("test")
            queue.send(storage, GMLStorageSeed(
                uri(1),
                Set(uri(1), uri(2), uri(3)),
                Vector(1 -> 1.0))
            )

            queue.send(storage, GMLStorageEstimator(
                new SemanticEstimator(Vector(1 -> 3.0))
            ))

            Thread.sleep(300)
    
            assert((cfg.path / "network.gml").readLines.mkString == net1)

            queue.send(storage, GMLStorageSeed(
                uri(2),
                Set(uri(1), uri(2), uri(3)),
                Vector(2 -> 2.0)
            ))

            queue.send(storage, GMLStorageEstimator(
                new SemanticEstimator(Vector(1 -> 3.0))
            ))

            Thread.sleep(300)

            assert((cfg.path / "network.gml").readLines.mkString == net2)
            
            watch(storage)
            queue.send(storage, EvaluatePriorityMatrixStop)
            expectTerminated(storage)
        }
    }
}
