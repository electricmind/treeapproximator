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
import LinkedVectorsStorage._

class TestLinkedVectorsStorage extends TestKit(ActorSystem("TestStorage"))
        with Tools
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import GMLStorage._

    implicit val cfg = CFG(path = new File("/tmp/test"), isdebug = false)

    val matrix1 = (new File(".") / "data" / "matrix1.dat").readLines.mkString

    val matrix2 = (new File(".") / "data" / "matrix2.dat").readLines.mkString

    val vd1 = (new File(".") / "data" / "1.dat").readLines.mkString
    val vd2 = (new File(".") / "data" / "2.dat").readLines.mkString

    val mids1 = (new File(".") / "data" / "map1.lst").readLines.mkString
    val mids2 = (new File(".") / "data" / "map2.lst").readLines.mkString

    "A linkedvectorstorage" should {
        "dump a vector" in {

            val gather = TestProbe()
            val seedqueue = TestProbe()

            val storage = system.actorOf(
                LinkedVectorsStorage.props(cfg), "TestLinkedVectorStorage")

            val data = cfg.path / "vectors"
            
            data / "matrix.dat" write("")
            data / "1.dat" write("test")
            data / "1.dat" write("test")
            data / "map.lst" write("")

            gather.send(storage, LinkedVectorsStorageSeed(
                uri(1),
                Set(uri(1), uri(2), uri(3)),
                Vector("one" -> 1.0))
            )

            Thread.sleep(300)

            assert((data / "matrix.dat").readLines.mkString == matrix1)
            assert((data / "1.dat").readLines.mkString == vd1)
            assert((data / "map.lst").readLines.mkString == mids1)

            gather.send(storage, LinkedVectorsStorageSeed(
                uri(2),
                Set(uri(3), uri(4)),
                Vector("two" -> 2.0)
            ))

            Thread.sleep(300)

            assert((data / "matrix.dat").readLines.mkString == matrix2)
            assert((data / "1.dat").readLines.mkString == vd1)
            assert((data / "2.dat").readLines.mkString == vd2)
            assert((data / "map.lst").readLines.mkString == mids2)

            assert(matrix1 != matrix2)
            assert(mids1 != mids2)

            watch(storage)
            gather.send(storage, EvaluatePriorityMatrixStop)
            expectTerminated(storage)
        }
    }
}
