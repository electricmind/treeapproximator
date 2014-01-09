package test
import org.scalatest.FlatSpec
import ru.wordmetrix.treeapproximator.TreeApproximator
import ru.wordmetrix.vector.Vector
import org.scalatest.Matchers
import ru.wordmetrix.webcrawler._
import java.net.URL
import java.net.URI
import scala.util.Random.{ nextBoolean, nextGaussian }
import Math.abs
class testCluster extends FlatSpec with Matchers {
// TODO: Move test vectors and clouds into separate file
    val v1 = Vector(1 -> 1, 4 -> 0)
    val v10 = Vector(1 -> 0, 4 -> 1)
    val v11 = Vector(1 -> 1, 4 -> 1)
    val v21 = Vector(1 -> 1, 5 -> 1)
    val v2 = Vector(2 -> 1, 4 -> 0)
    val v12 = Vector(2 -> 1, 4 -> 1)
    val v3 = Vector(1 -> 1, 2 -> 1)
    val v4 = Vector(3 -> 1, 4 -> 0)
    val v14 = Vector(3 -> 1, 4 -> 1)
    val v5 = Vector(1 -> 1, 3 -> 1)
    val v6 = Vector(2 -> 1, 3 -> 1)
    val v16 = Vector(2 -> 1, 3 -> 1, 4 -> 1)
    val v7 = Vector(1 -> 1, 2 -> 1, 3 -> 1)
    val v30 = Vector(4 -> 1, 5 -> 1)
    val v60 = Vector(5 -> 1, 6 -> 1)
    val v31 = Vector(1 -> 1, 4 -> 1, 5 -> 1)
    val v32 = Vector(2 -> 1, 4 -> 1, 5 -> 1)

    type V = scala.collection.immutable.Vector[Vector[Int]]
    val V = scala.collection.immutable.Vector

    def dist[F](vs: Vector[F]*) = vs.toList.sliding(2) map {
        case v1 :: v2 :: _ => (v1 - v2).sqr
    }

    def av[F](vs: Vector[F]*) = dist(vs: _*).toList match {
        case ds => ds.sum / ds.length
    }

    def d[F](vs: Vector[F]*) = (dist(vs: _*).toList, av(vs: _*)) match {
        case (ds, av) => ds.map(x => Math.pow(x - av, 2)).sum / ds.length
    }

    "A cluster" should "be created" in {
        new Cluster().vector should be(V())
        new Cluster().dispersion should be(0.0)

        new Cluster(v1).vector should be(V(v1))
        new Cluster(v1).dispersion should be(0.0)
        new Cluster(v1).squares should be(0.0)

        new Cluster(v1, v2).vector should be(V(v1, v2))
        new Cluster(v1, v2).dispersion should
            be((v1 - v2).sqr plusOrMinus 1e-10)
        new Cluster(v1, v2).squares should be((v1 - v2).sqr)

        new Cluster(v1, v2, v3).vector should be(V(v1, v2, v3))
        new Cluster(v1, v2, v3).squares should be(dist(v1, v2, v3).sum)

        new Cluster(v1, v2, v3, v4).vector should be(V(v1, v2, v3, v4))
        new Cluster(v1, v2, v3, v4).squares should
            be(dist(v1, v2, v3, v4).sum plusOrMinus 0.00001)
        new Cluster(v1, v2, v3, v4).dispersion should
            be(dist(v1, v2, v3, v4).sum / 3 plusOrMinus 0.00001)
    }

    "A cluster" should "be extendable with :+" in {
        (new Cluster(v1) :+ v2).vector should be(V(v1, v2))
        (new Cluster(v1) :+ v2).dispersion should
            be((v1 - v2).sqr plusOrMinus 1e-10)
        (new Cluster(v1) :+ v2).squares should
            be((v1 - v2).sqr plusOrMinus 1e-10)

        (new Cluster(v1) :+ v2 :+ v3).vector should be(V(v1, v2, v3))
        (new Cluster(v1) :+ v2 :+ v3).squares should
            be(dist(v1, v2, v3).sum plusOrMinus 1e-10)
        (new Cluster(v1) :+ v2 :+ v3).dispersion should
            be(dist(v1, v2, v3).sum / 2 plusOrMinus 0.0001)

        (new Cluster(v1) :+ v2 :+ v3 :+ v4).vector should be(V(v1, v2, v3, v4))
        (new Cluster(v1) :+ v2 :+ v3 :+ v4).squares should
            be(dist(v1, v2, v3, v4).sum plusOrMinus 0.00001)
        (new Cluster(v1) :+ v2 :+ v3 :+ v4).dispersion should
            be(dist(v1, v2, v3, v4).sum / 3 plusOrMinus 0.00001)

    }

    "A cluster" should "be extendable with +:" in {

        (v1 +: new Cluster(v2)).vector should be(V(v1, v2))
        (v1 +: new Cluster(v2)).dispersion should
            be((v1 - v2).sqr plusOrMinus 1e-10)
        (v1 +: new Cluster(v2)).squares should
            be((v1 - v2).sqr plusOrMinus 1e-10)

        (v1 +: v2 +: new Cluster(v3)).vector should be(V(v1, v2, v3))
        (v1 +: v2 +: new Cluster(v3)).squares should
            be(dist(v1, v2, v3).sum plusOrMinus 0.0001)
        (v1 +: v2 +: new Cluster(v3)).dispersion should
            be(dist(v1, v2, v3).sum / 2 plusOrMinus 0.0001)

        (v1 +: v2 +: v3 +: new Cluster(v4)).vector should be(V(v1, v2, v3, v4))
        (v1 +: v2 +: v3 +: new Cluster(v4)).squares should
            be(dist(v1, v2, v3, v4).sum plusOrMinus 0.00001)
        (v1 +: v2 +: v3 +: new Cluster(v4)).dispersion should
            be(dist(v1, v2, v3, v4).sum / 3 plusOrMinus 0.00001)
    }

    "A cluster" should "have union method" in {
        (new Cluster(v1) union new Cluster(v2)).vector should be(V(v1, v2))
        (new Cluster(v1) union new Cluster(v2)).dispersion should
            be((v1 - v2).sqr plusOrMinus 1e-10)
        (new Cluster(v1) union new Cluster(v2)).squares should
            be((v1 - v2).sqr plusOrMinus 1e-10)

        (new Cluster(V(v1, v2)) union new Cluster(v3)).vector should
            be(V(v1, v2, v3))
        (new Cluster(V(v1, v2)) union new Cluster(v3)).squares should
            be(dist(v1, v2, v3).sum plusOrMinus 0.00001)
        (new Cluster(V(v1, v2)) union new Cluster(v3)).dispersion should
            be(dist(v1, v2, v3).sum / 2 plusOrMinus 0.00001)

        (new Cluster(V(v1, v2)) union new Cluster(V(v3, v4))).vector should
            be(V(v1, v2, v3, v4))
        (new Cluster(V(v1, v2)) union new Cluster(V(v3, v4))).squares should
            be(dist(v1, v2, v3, v4).sum plusOrMinus 0.00001)
        (new Cluster(V(v1, v2)) union new Cluster(V(v3, v4))).dispersion should
            be(dist(v1, v2, v3, v4).sum / 3 plusOrMinus 0.00001)
    }

    "A clusters" should "union if check passed" in {
        println(new Cluster(v2, v1, v3).last)
        println(new Cluster(v2, v1, v3).head)
        println(new Cluster(v3, v2, v3).squares)

        (new Cluster(v1, v3)
            unionIfCheck new Cluster(v3, v2)) should not be (None)
        (new Cluster(v1, v3)
            unionIfCheck new Cluster(v14, v4)) should be(None)

        (new Cluster(v1, v3, v2)
            unionIfCheck new Cluster(v2, v6, v4)) should not be (None)
        (new Cluster(v1, v3, v2)
            unionIfCheck new Cluster(v6, v4, v12)) should not be (None)

        (new Cluster(v3, v3, v3, v1)
            unionIfCheck new Cluster(v2, v3, v3, v3)) should be(None)
        (new Cluster(v1, v1, v3, v3, v2)
            unionIfCheck new Cluster(v1, v3, v3, v2, v2)) should be(None)

        (new Cluster(v6, v2, v3)
            unionIfCheck new Cluster(v14, v4, v6)) should be(None)
        (new Cluster(v3, v2, v6)
            unionIfCheck new Cluster(v11, v1, v3)) should be(None)

        (new Cluster(v1, v1, v3)
            unionIfCheck new Cluster(v6, v16, v16)) should be(None)

        (new Cluster(v1, v2, v3)
            unionIfCheck new Cluster(v4, v10, v14)) should be(None)
    }

    "A trivial cluster" should "pass the union test" in {
        new Cluster().check(10) should be(true)
        new Cluster(v1).check(10) should be(true)
        new Cluster(v1, v2).check(10) should be(false)
    }

    "A simple chain" should "be clustered" in {
        Clusters(List[Vector[Int]]()).size should be(0)
        //        Clusters(List(v1)).size should be(1)
        println(5)
        Clusters(List(v1, v2)).size should be(1)
        println(6)
        Clusters(List(v1, v2, v4)).size should be(1)
        Clusters(List(v1, v2, v4, v10)).size should be(1)
        Clusters(List(v1, v2, v4, v10, v3)).size should be(1)
        Clusters(List(v1, v2, v3)).size should be(1)

        /*   println("1 2 3 =        " + Clusters(List(v1,v2,v3)).head)
        
        println("1 2 3 4 =      " + Clusters(List(v1,v2,v3, v4)).head)
        println("1 2 3 4 =      " + Clusters(List(v1,v2,v3, v4)))
    
        println("1 2 3 4 10 =   " + Clusters(List(v1,v2,v3, v4, v10)).head)
        println("1 2 3 4 10 =   " + Clusters(List(v1,v2,v3, v4, v10)).drop(1).head)
        
    */
        println("1 2 3 4 10 14")
        /*        val c1 :: c2 :: _ = Clusters(List(v1,v2,v3,v4,v10,v14)).toList
        println("c1 = " + c1)
        println("c2 = " + c2)*/

        Clusters(List(v1, v3, v3, v2, v4, v10, v10, v14)).size should be(2)
        val c1 :: c2 :: cs = Clusters(List(v1, v3, v3, v2, v4, v10, v10, v14)).toList
        println("c1 = " + c1)
        println("c2 = " + c2)
        println("cs = " + cs)
        Clusters(List(v1, v1, v2, v2, v4, v4, v10, v10, v3, v3, v14, v14)).size should be(6)
    }

    "A simple clouds" should "be clustered" in {
        val tree = List(v1, v3, v3, v2, v4, v10, v10, v14).zipWithIndex
            .foldLeft(TreeApproximator[Int, Int]())({
                case (tree, (v, va)) => tree + (v, va)
            }).rectify(4).align()._1
       Clusters(tree) foreach {
           case x => println(":: " + x)
       }
       Clusters(tree).size should be(2)
       
    }
}
