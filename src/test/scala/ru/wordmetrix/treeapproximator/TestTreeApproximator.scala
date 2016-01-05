package test
import ru.wordmetrix.vector.Vector
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ru.wordmetrix.webcrawler._
import java.net.URL
import java.net.URI
import scala.util.Random.{ nextBoolean, nextGaussian }
import Math.abs
import ru.wordmetrix.treeapproximator.TreeApproximatorNode
import ru.wordmetrix.treeapproximator.TreeApproximator
class testTreeApproximator extends FlatSpec with Matchers {
    implicit val accuracy = 0.0001d

    val v1 = Vector(1 -> 1, 4 -> 0)
    val v21 = Vector(1 -> 1, 5 -> 1)
    val v2 = Vector(2 -> 1, 4 -> 0)
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
    lazy val t12 = (TreeApproximator(v1 -> 1) + (v2, 2))
    lazy val t243 = (TreeApproximator(v2 -> 2) + (v4, 4) + (v3, 3))
    lazy val t630731 = (TreeApproximator(v6 -> 6) + (v30, 30) + (v31, 31) + (v7, 7))

    "An energy" should "be implemented" in {
        TreeApproximator[String, Int]().energy should be(0)
        TreeApproximator(v1 -> 1).energy should be(0)
        TreeApproximator(v1 -> 1, v2 -> 1).energy_ should be(2)
        TreeApproximator(v1 -> 1, v2 -> 1, v3 -> 1).energy_ should be(1 + 1 + 2)
        TreeApproximator(v1 -> 1, v2 -> 1, v3 -> 1, v4 -> 1).energy_ should
            be(1 + 1 + 2 + 3 + 2 + 2)

        TreeApproximator(v1 -> 1, v2 -> 1).energy should be(0.70 plusOrMinus 0.01)
        TreeApproximator(v1 -> 1, v2 -> 1, v3 -> 1).energy should
            be((1 + 1 + 2) / 6.0)
        /*TreeApproximator(v1 -> 1, v2 -> 1, v3 -> 1, v4 -> 1).energy should
            be((1 + 1 + 2 + 3 + 2 + 2) / 4.)*/
    }

    "A leaf " should "be created from one vector" in {
        TreeApproximator(v1 -> 1).average should be(v1)
        TreeApproximator(v1 -> 1).value should be(1)

        TreeApproximator(v2 -> 2).average should be(v2)
        TreeApproximator(v2 -> 2).value should be(2)
    }

    "A leaf" should "return the same vector" in {
        TreeApproximator(v1 -> 1)(Ordering[Int], accuracy)(v1) should be(1)
        TreeApproximator(v1 -> 1)(Ordering[Int], accuracy)(v2) should be(1)
        TreeApproximator(v1 -> 1)(Ordering[Int], accuracy)(v3) should be(1)

        TreeApproximator(v2 -> 2)(Ordering[Int], accuracy)(v1) should be(2)
        TreeApproximator(v2 -> 2)(Ordering[Int], accuracy)(v2) should be(2)
        TreeApproximator(v2 -> 2)(Ordering[Int], accuracy)(v3) should be(2)
    }

    "A node" should "be created from two vectors" in {

        t12.average should be(v1 + v2)
        t12.n should be(2)

        (t12 / 1).value should be(1)
        (t12 / 2).value should be(2)

        (t12 / 1).average should be(v1)
        (t12 / 2).average should be(v2)
    }

    "A node's nearest" should "return nearest leaf" in {
        val t12 = (TreeApproximator(v1 -> 1) + (v2, 2)).asInstanceOf[TreeApproximatorNode[Int, Int]]
        t12.nearest(v1).head.average should be(v1)
        t12.nearest(v1).head.value should be(1)
    }

    "A tree-3" should "be created from tree vectors" in {
        t243.average should be(v2 + v4 + v3)

        t243.n should be(3)

        (t243 / 2).value should be(4)

        (t243 / 1 / 1).value should be(2)

        (t243 / 1 / 2).value should be(3)

        (t243 / 2).average should be(v4)
        (t243 / 1).average should be(v2 + v3)
    }

    "A tree-4" should "be created from four vectors" in {
        t630731.average should be(v6 + v30 + v7 + v31)
        t630731.n should be(4)
        (t630731 / 1).n should be(2)
        (t630731 / 2).n should be(2)

        (t630731 / 1 / 1).value should be(6)
        (t630731 / 1 / 2).value should be(7)
        (t630731 / 2 / 1).value should be(30)
        (t630731 / 2 / 2).value should be(31)

        (t630731 / 1).average should be(v6 + v7)
        (t630731 / 2).average should be(v30 + v31)
    }

    "A tree-4" should "make right look up" in {
        val epsilon = Vector(1 -> 0.1, 2 -> 0.1, 3 -> 0.1, 4 -> 0.1)
        t630731(v6) should be(6)
        t630731(v7) should be(7)
        t630731(v30) should be(30)
        t630731(v31) should be(31)

        t630731(v6 + epsilon) should be(6)
        t630731(v7 + epsilon) should be(7)
        t630731(v30 + epsilon) should be(30)
        t630731(v31 + epsilon) should be(31)

        t630731(v16) should be(6)
        t630731(v3) should be(7)
        t630731(v32) should be(30)
        t630731(v21) should be(31)
    }

    def generate2tree1(d: Double, n: Int) = (1 until n).map(x => 2 * (x % 2) - 1).map(
        x => (Vector(1 -> (nextGaussian * d + x), 2 -> (nextGaussian * d + x)).normal, x)
    ).foldLeft(TreeApproximator[Int, Int]((Vector[Int](1 -> 1d), 0)))({
            case (tree, (key, value)) =>
                //                    println(key); 
                tree + (key, value)
        })

    def twoheadstest(d: Double, n: Int, gen: (Double, Int) => TreeApproximator[Int, Int]) = {
        val tree = gen(d, n)

        tree.n should be(n)
        (tree / 1).n.toDouble should be((n / 2d) plusOrMinus n / 10d)
        (tree / 2).n.toDouble should be((n / 2d) plusOrMinus n / 10d)
        ((tree / 1).average / (tree / 1).n).norm should be(0.64 plusOrMinus 0.05)
        ((tree / 1).average - (tree / 2).average).norm should be((0.64d * n) plusOrMinus n / 10d)

         
        tree.energy should be(0.999 plusOrMinus 0.01)
        (tree / 1).energy should be(0.7920 plusOrMinus 0.1)
        (tree / 2).energy should be(0.7920 plusOrMinus 0.1)
    }

    "A random mixing tree" should "has two heads" in {
        val d = 100
        val n = 1000
        twoheadstest(d, n, generate2tree1)
    }

    def generate2tree2(d: Double, n: Int) = ((1 until n / 2).map(x => 1) ++ ((1 to n / 2).map(x => -1))).map(
        x => (Vector(1 -> (nextGaussian * d + x), 2 -> (nextGaussian * d + x)).normal, 1)
    ).foldLeft(TreeApproximator[Int, Int]((Vector[Int](1 -> 1d), 0)))({
            case (tree, (key, value)) =>
                tree + (key, value)
        })

    "A random sequence tree" should "has two heads" in {
        val d = 100
        val n = 1000
        twoheadstest(d, n, generate2tree2)
    }

    "A rectify" should "save leaves" in {
        val n = 100
        val tree1 = generate2tree1(100, n)
        val tree2 = tree1.rectify(100)
        tree2.n should be(tree1.n)
        tree2.energy should be(tree1.energy plusOrMinus 0.001)
    }

    "Next stages each" should "be better than previous ones" in {
        val n = 1000
        val tree1 = generate2tree1(100, n)
        val (tree2, e1) = (tree1.rectify(n), tree1.energy2)
        val (tree3, e2) = (tree2.rectify(n), tree2.energy2)
        val (tree4, e3) = (tree3.rectify(n), tree3.energy2)
        val (tree5, e4) = (tree4.rectify(n), tree4.energy2)
        val (tree6, e5) = (tree5.rectify(n), tree5.energy2)
        val e6 = tree6.energy2

        println("Energy: %4.3f %4.3f %4.3f %4.3f %4.3f %4.3f".format(
            e1, e2, e3, e4, e5, e6))

        e2 should be < e1
        e3 should be < e2
        e4 should be < e3
        e5 should be < e4
        e6 should be < e5
    }

    "A rectify" should "save two heads in mixing sample" in {
        val n = 1000
        var tree = generate2tree1(100, n)
        for (i <- 1 to 10) {
            tree = tree.rectify(n)
            println(tree.n + " " + tree.energy2)
        }
        twoheadstest(100, n, { case (x, y) => tree })
    }

    "A rectify" should "save two heads in sequence sample" in {
        val n = 1000
        var tree = generate2tree2(100, n)
        for (i <- 1 to 10) {
            tree = tree.rectify(n)
            println(tree.n + " " + tree.energy2)
        }
        twoheadstest(100, n, { case (x, y) => tree })
    }

    //    "A leaf" should "have 1 items iterator" in {
    //        TreeApproximator(v1 -> 1).toIterator.size should be(1)
    //        println(TreeApproximator(v1 -> 1))
    //    }
    "A leaf" should "have 1 items iterator" in {
        TreeApproximator(Vector("A" -> 1.0) -> 1).size should be(1)
    }

    "A node" should "have 2 items iterator" in {
        t12.toIterator.size should be(2)
    }

    "A 3-tree" should "have 3 item iterator" in {
        val it = t243.toIterator
        t243.toIterator.size should be(3)
    }

    "A 4-tree" should "have 4 item iterator" in {
        t630731.toIterator.size should be(4)
    }

    "An align" should "align" in {
        val tree = (TreeApproximator(v3 -> 3) + (v6, 6) + (v14, 14) + (v30, 30) + (v60, 60)).rectify(10).align()._1
        println(tree.map(_._2))
        println(tree / 1 / 1 average)
        println(tree / 1 / 2 average)
        println(tree / 2 / 1 average)
        println(tree / 2 / 2 average)
        println(tree / 2 / 2 / 1 average)
        println(tree / 2 / 2 / 2 average)
        for ((average, _) <- tree) {
            println(average)
        }
    }
}
