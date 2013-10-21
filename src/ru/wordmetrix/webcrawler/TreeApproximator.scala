package ru.wordmetrix.webcrawler

import scala.collection

import TreeApproximator._
import scala.annotation.tailrec

//TODO: add diversity computation 
//TODO: Compute energy during changing an item
//TODO: make empty tree :) to simplify initialization

object TreeApproximator {

    type Node[F, V] = TreeApproximatorNode[F, V]
    type Leaf[F, V] = TreeApproximatorLeaf[F, V]
    type Tree[F, V] = TreeApproximator[F, V]
    type State[F, V] = (Option[Leaf[F, V]], List[Tree[F, V]])

    def apply[F, V](vs: (Vector[F], V)*) = vs.toList match {
        case (v, value) :: vs => vs.foldLeft[TreeApproximator[F, V]](new TreeApproximatorLeaf[F, V](v, value))({
            case (tree, (vector, value)) => tree + (vector, value)
        })
    }

    class Iterator[F, V](root: Tree[F, V]) extends collection.Iterator[Leaf[F, V]] {
        println("create iterator")
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

            next
        }
    }
}

trait TreeApproximator[F, V] {
    val average: Vector[F] // = null //TODO : Create an ability to have empty vectors

    def +(vector: Vector[F], value: V): TreeApproximatorNode[F, V] //= new TreeApproximatorLeaf(vector,value)
    val n: Int // = 0
    def apply(average: Vector[F]): V
    def energy: Double
    def energy2 : Double
    def iterator: collection.Iterator[TreeApproximator.Leaf[F, V]] =
        new TreeApproximator.Iterator[F, V](this)

    def /(n: Int): TreeApproximator[F, V]

    def path(vector: Vector[F]): Stream[Int]

    def value: V

    //    def reinsert(): TreeApproximator.Leaf[F, V]

    // TODO: implement align method.
    def align(v : Vector[F]) : (Tree[F,V],Vector[F])
    def align()(implicit ord : Ordering[F]) : (Tree[F,V],Vector[F]) = align(Vector[F]())
}

class TreeApproximatorNode[F, V](val child1: TreeApproximator[F, V],
                                 val child2: TreeApproximator[F, V])
        extends TreeApproximator[F, V]
        with Iterable[TreeApproximator.Leaf[F, V]] {
    val average = child1.average + child2.average

    val n = child1.n + child2.n

    def nearest(vector: Vector[F]) = List(child1, child2).map(xv =>
        (vector * xv.average.normal, xv)).sortBy(-_._1).map(_._2)

    def +(vector: Vector[F], value: V) = {
        nearest(vector) match {
            case x :: y :: List() => new TreeApproximatorNode[F, V](
                x + (vector, value), y
            )
        }
    }
    def apply(vector: Vector[F]): V = nearest(vector).head(vector)

    def energy = {
        this.iterator.toList.combinations(2).foldLeft((0, 0d))({
            case ((n, e), x :: y :: List()) =>
                (n + 1, e + (x.average - y.average).norm)
        }) match { case (e, n) => e / n }
    }

    def energy2 = {
        (child1.average.normal-child2.average.normal).norm + child1.energy2 + child2.energy2
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

    def random(): (Leaf[F, V], Tree[F, V]) = {
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

    def align(vector : Vector[F]) : (Tree[F,V],Vector[F]) = {
        val (c1, v1) = child1.align(vector)
        val (c2, v2) = child2.align(v1)
        
        List((v1,c1),(v2,c2)).sortBy(_._1.normal*vector.normal) match {
            case List((v2,c2),(v1,c1)) => (new Node[F,V](c1,c2),v2)
        }
    } 
    
    @tailrec
    final def rectify(n: Int = 1): Node[F,V] = if (n > 0) this.random() match {
        case (leaf, tree) => (tree + (leaf.average, leaf.value)).rectify(n - 1)
    } else this
}

class TreeApproximatorLeaf[F, V](val average: Vector[F], val value: V)
        extends TreeApproximator[F, V] {
    def +(vector: Vector[F], value: V) = new TreeApproximatorNode[F, V](
        this, new TreeApproximatorLeaf[F, V](vector, value))
    val n = 1

    def apply(vector: Vector[F]): V = value
    def energy = 0.0
    def energy2 = 0.0
    def /(n: Int) = this
    def path(vector: Vector[F]): Stream[Int] = Stream()
    def reinsert(): TreeApproximator.Leaf[F, V] = this
    //    override def toString = average.toString
    def align(vector : Vector[F]) : (Tree[F,V],Vector[F]) = (this,average)
}
