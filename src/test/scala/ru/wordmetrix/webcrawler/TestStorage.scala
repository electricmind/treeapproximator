package ru.wordmetrix.webcrawler

import java.net.URI
import scala.concurrent.duration.DurationInt
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import Gather.GatherIntel
import akka.actor.ActorSystem
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit, TestProbe}
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.webcrawler.Storage.{StorageCompleted, StorageSign, StorageVictim}
import akka.actor.PoisonPill


class TestStorage extends TestKit(ActorSystem("TestStorage"))

        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    val cfg = CFG(List("-limit","99","",""))

    "A storage" should {
        "save a page" in {

            val queue = TestProbe()
            val gather = TestProbe()
            val sample = TestProbe()
            val seedqueue = TestProbe()

            val storage = system.actorOf(
                Storage.props(cfg),
                "TestStorage")

            val uri = new URI("http://example.org")

            queue.send(storage, StorageVictim(seedqueue.ref))
            gather.send(storage, GatherIntel(uri, "Test Test Test Test"))
            queue.send(storage, StorageSign(uri))
        }
    }
   
   
   "A storage" should {
        "send kill message" in {
            val queue = TestProbe()
            val gather = TestProbe()
            val sample = TestProbe()
            val seedqueue = TestProbe()

            val storage = system.actorOf(
                Storage.props(cfg),
                "TestStorageKiller")

            val uri = new URI("http://example.org")
            watch(seedqueue.ref)
            queue.send(storage, StorageVictim(seedqueue.ref))
            
            for ( i <- 1 to 100 ) {
                gather.send(storage, GatherIntel(uri, "Test Test Test Test"))
            }
            
            queue.send(storage, StorageSign(uri))
            expectTerminated(seedqueue.ref)
            //seedqueue.expectMsg(PoisonPill)
            
        }
    }

}
