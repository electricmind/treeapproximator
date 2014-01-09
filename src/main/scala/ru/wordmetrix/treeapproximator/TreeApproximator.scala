package ru.wordmetrix.treeapproximator
import ru.wordmetrix.vector._
import TreeApproximator._
import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper

//TODO: add diversity computation 
//TODO: Compute energy during changing an item

//TODO: Find root of cluster

object TreeApproximator {

    type Node[F, V] = TreeApproximatorNode[F, V]
    type Empty[F, V] = TreeApproximatorEmpty[F, V]
    type Leaf[F, V] = TreeApproximatorLeaf[F, V]
    type Tree[F, V] = TreeApproximator[F, V]
    type State[F, V] = (Option[Leaf[F, V]], List[Tree[F, V]])

    def apply[F, V](vs: (Vector[F], V)*)(implicit ord: Ordering[F]): Tree[F, V] =
        vs.foldLeft[TreeApproximator[F, V]](new TreeApproximatorEmpty[F, V]())({
            case (tree, (vector, value)) => tree + (vector, value)
        })

    class Iterator[F, V](root: Tree[F, V]) extends collection.Iterator[(Vector[F], V)] {
        var state: State[F, V] = root match {
            case node: Node[F, V] => godown(node)
            case leaf: Leaf[F, V] => (Some(leaf), List())
        }

        def godown(node: Tree[F, V], stack: List[Tree[F, V]] = List()): State[F, V] = node match {
            case node: Node[F, V] =>
                godown(node.child1, node.child2 :: stack)

            case leaf: Leaf[F, V] => (Some(leaf), stack)
        }

        def headOption = state._1

        def hasNext() = headOption.exists(x => true)

        def next() = {
            val (Some(next), stack) = state

            state = stack match {
                case (leaf: Leaf[F, V]) :: stack => (Some(leaf), stack)

                case (node: Node[F, V]) :: stack =>
                    godown(node, stack)

                case List() =>
                    (None, List())
            }

            (next.average, next.value)
        }
    }

    /*trait Energy {
        val energy : Double
    }*/

    /*def evaluateEnergy[F,V](tree : Tree[F,V])(implicit ord : Ordering[F]) : TreeE[F,V] = tree match {
        case empty : Empty[F,V] => new EmptyE(empty)
        
    }
    
    trait TreeE[F,V] extends Tree[F,V] with Energy {
        val node : Tree[F,V]
    }
    
    class NodeE[F,V](val child1: TreeApproximator[F, V],
                                 val child2: TreeApproximator[F, V])(implicit ord : Ordering[F]) extends Node[F,V](child1,child2) with TreeE[F,V] {
        
    }
    
    class EmptyE[F,V](val node : Tree[F,V])(implicit ord : Ordering[F]) extends Empty[F,V] with TreeE[F,V] {
        
    }
    */

}

trait TreeApproximator[F, V] extends Iterable[(Vector[F], V)] with Serializable {
    implicit val ord: Ordering[F]
    val average: Vector[F]

    def bind(
        clusters: Map[Vector[F], Iterable[Vector[F]]]): Tree[F, Iterable[Vector[F]]] =
        bindleaf({ case (v, va) => clusters(v) })

    def bind[R](map: V => R): Tree[F, R] =
        bindleaf[R]({ case (v, va) => map(va) })

    def bindleaf[R](map: (Vector[F], V) => R): Tree[F, R] = {
        this match {
            case empty: Empty[F, V] => new Empty[F, R]()
            case leaf: Leaf[F, V] =>
                new Leaf[F, R](leaf.average, map(leaf.average, leaf.value))

            case node: Node[F, V] => new Node[F, R](
                node.child1.bindleaf(map),
                node.child2.bindleaf(map)
            )
        }
    }

    def +(vector: Vector[F], value: V): Tree[F, V] //= new TreeApproximatorLeaf(vector,value)
    val n: Int // = 0
    //    def apply(average: Vector[F]): V

    def apply(average: Vector[F]) = get[V](x => x.value)(average)
    def leaf = get[Leaf[F, V]](x => x) _

    @tailrec
    final def get[R](f: Leaf[F, V] => R)(vector: Vector[F]): R = this match {
        case _: Empty[F, V] =>
            throw new java.lang.IndexOutOfBoundsException()
        case leaf: Leaf[F, V] => f(leaf)

        case node: Node[F, V] => node.nearest(vector).head.get[R](f)(vector)
    }

    def energy = Math.sqrt(energy_) / n // / n
    def energy_ : Double

    def energy2: Double

    def dispersion: Double

    def iterator: collection.Iterator[(Vector[F], V)] =
        new TreeApproximator.Iterator[F, V](this)

    def /(n: Int): TreeApproximator[F, V]

    def path(vector: Vector[F]): Stream[Int]

    def value: V

    def rectify(n: Int): Tree[F, V] // = this
    def align(v: Vector[F]): (Tree[F, V], Vector[F])
    def align()(implicit ord: Ordering[F]): (Tree[F, V], Vector[F]) = align(Vector[F]())

    def cluster(v: Vector[F]) = {
        val path: Stream[Int] = this.path(v)
        def estimate(tree: Tree[F, V], path: Stream[Int]): (Boolean, Tree[F, V], Double) = {
            path match {
                case n #:: m1 #:: m2 #:: m3 #:: Stream() => (true, tree, tree.energy)
                case n #:: path =>
                    val qq = estimate(tree / n, path)
                    //println("!!!!",path,qq._1,qq._3,tree.n, tree.energy, tree.energy / qq._3)
                    qq match {
                        case (true, t, e) if 0.8 < tree.energy / e => (false, t, e)
                        case (true, t, e)                          => (true, tree, tree.energy)
                        case (false, t, e)                         => (false, t, e)
                    }
            }
        }
        //        println(path.mkString(" / "))
        estimate(this, path)._2
    }

}

class TreeApproximatorEmpty[F, V](implicit val ord: Ordering[F]) extends TreeApproximator[F, V] {
    def /(n: Int): Tree[F, V] = this
    def +(vector: Vector[F], value: V): Tree[F, V] = new Leaf(vector, value)
    def align(v: Vector[F]): (Tree[F, V], Vector[F]) = (this, Vector[F]())
    //    def apply(average: Vector[F]): V = value
    val average: Vector[F] = Vector[F]()
    
    override def energy = 0d
    def energy_ : Double = 0.0d
    def dispersion = 0.0d
    def energy2: Double = 0d
    val n: Int = 0
    def path(vector: Vector[F]): Stream[Int] = Stream()
    def value: V = ???
    def rectify(n: Int) = this
}

class TreeApproximatorNode[F, V](val child1: TreeApproximator[F, V],
                                 val child2: TreeApproximator[F, V])(implicit val ord: Ordering[F])
        extends TreeApproximator[F, V] {
    val average = (child1.average + child2.average) clearMinors (4000)

    val n = child1.n + child2.n

    val dispersion: Double = (child1, child2) match {
        case (c1: Leaf[F, V], c2: Leaf[F, V]) => (c1.average - c2.average).sqr
        //        case (leaf: Leaf[F, V], node: Node[F, V]) =>
        //            (leaf.average - node.average).sqr + node.dispersion

        //        case (node: Node[F, V], leaf: Leaf[F, V]) =>
        //            (leaf.average - node.average).sqr + node.dispersion

        case (c1, c2)                         => (c1.dispersion * c1.n + c2.dispersion * c2.n) / n
    }
    def nearest(vector: Vector[F]) = List(child1, child2).map(xv =>
        (vector * xv.average.normal, xv)).sortBy(-_._1).map(_._2)

    def +(vector: Vector[F], value: V) = {
        nearest(vector) match {
            case x :: y :: List() => new TreeApproximatorNode[F, V](
                x + (vector, value), y
            )
        }
    }
    //    def apply(vector: Vector[F]): V = nearest(vector).head(vector)

    lazy val energy_ = (
        child1.energy_ +
        child2.energy_ + {
            for ((average1, _) <- child1; (average2, _) <- child2)
                yield (average1 - average2).sqr
        }.sum
    )

    def energy2 = {
        (child1.average.normal - child2.average.normal).norm + child1.energy2 + child2.energy2
    }

    def /(n: Int) = n match {
        case 1 => child1
        case 2 => child2
    }

    def path(vector: Vector[F]): Stream[Int] = nearest(vector).head match {
        case x if x == child1 => 1 #:: x.path(vector)
        case x                => 2 #:: x.path(vector)
    }

    def value = throw new java.lang.ArrayIndexOutOfBoundsException()

    def align(vector: Vector[F]): (Tree[F, V], Vector[F]) = {

        val List(cd2, cd1) = List(child1, child2).sortBy(_.average.normal * vector.normal)

        val (c1, v1) = cd1.align(vector)
        val (c2, v2) = cd2.align(v1)

        (new Node[F, V](c1, c2), v2)

        /*        List((v1, c1), (v2, c2)).sortBy(_._1.normal * vector.normal) match {
            case List((v2, c2), (v1, c1)) => (new Node[F, V](c1, c2), v2)
        }
*/ }

    final def random(): (Tree[F, V], Tree[F, V]) = {
        if (scala.util.Random.nextInt(n) < child1.n) {
            child1 match {

                case leaf: Leaf[F, V] => (leaf, child2)
                case node: Node[F, V] => node.random() match {
                    case (leaf, node) => (leaf, new Node[F, V](node, child2))
                }
            }
        } else {
            child2 match {
                case leaf: Leaf[F, V] => (leaf, child1)
                case node: Node[F, V] => node.random() match {
                    case (leaf, node) => (leaf, new Node[F, V](child1, node))
                }
            }
        }
    }

    @tailrec
    final def rectify(n: Int = 1): Node[F, V] = if (n > 0) this.random() match {
        case (leaf, tree) => (tree + (leaf.average, leaf.value)) match {
            case node: Node[F, V] => node.rectify(n - 1)
        }
    }
    else this
}

class TreeApproximatorLeaf[F, V](
    val average: Vector[F], val value: V)(
        implicit val ord: Ordering[F])
        extends TreeApproximator[F, V] {
    def +(vector: Vector[F], value: V) = new TreeApproximatorNode[F, V](
        this, new TreeApproximatorLeaf[F, V](vector, value))
    val n = 1

    def dispersion = 0.0d

    //    def apply(vector: Vector[F]): V = value
    def energy_ = 0.0
    def energy2 = 0.0
    def /(n: Int) = this
    def path(vector: Vector[F]): Stream[Int] = Stream()
    def reinsert(): TreeApproximator.Leaf[F, V] = this
    //    override def toString = average.toString
    def align(vector: Vector[F]): (Tree[F, V], Vector[F]) = (this, average)
    def rectify(n: Int) = this
}
