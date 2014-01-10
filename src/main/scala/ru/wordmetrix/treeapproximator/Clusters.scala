package ru.wordmetrix.treeapproximator

import ru.wordmetrix.vector.Vector
import scala.Option.option2Iterable

import scala.collection.TraversableOnce.flattenTraversableOnce
object Clusters {
    type V[V] = scala.collection.immutable.Vector[V]
    def V = scala.collection.immutable.Vector

    type Item[F, V] = (Vector[F], V)
    type Pair[F, V] = (Item[F, V], Item[F, V])

    private def fromPairs[F, V](pairs: Iterable[Pair[F, V]]) = pairs.foldLeft(new Clusters[F]()) {
        case (cs, ((v1, va1), (v2, va2))) if v1 != v2 =>
            cs + (v1, v2)
            
        case (cs, ((v1, va1), (v2, va2))) =>
            println("pairing failed", va1, va2)
            cs

    }

    def apply[F, V](tree: Iterable[Item[F, V]]): Clusters[F] = {
        fromPairs(pairs(tree))
    }

    def apply[F](chain: Seq[Vector[F]]): Clusters[F] = apply(chain.zipWithIndex)

    def pairs[F, V](tree: Iterable[(Vector[F], V)]) =  (tree.foldLeft((Set[Vector[F]](),List[(Vector[F], V)]())) {
            case ((set,list),(k,v)) => if (set contains k) (set, list) else {
                (set + k, (k,v) :: list)
            }
        })._2.reverse.sliding(2).map({
            case (v1, value1) :: (v2, value2) :: _ =>
                Some(((v1 - v2).sqr, ((v1, value1), (v2, value2))))
            case _ => None
        }).flatten.toList.sortBy(_._1).map(_._2).toSeq
    
    
    def pairs[F](vectors: Seq[Vector[F]]) = {
        vectors.sliding(2).filter(_.length == 2).map {
            case scala.collection.immutable.Vector(v1, v2) => (v1, v2)
        }
    }
    def average(x: V[Double]) = x.sum
}

class Clusters[F](
    val heads: Map[Vector[F], Cluster[F]] = Map[Vector[F], Cluster[F]](),
    val lasts: Map[Vector[F], Cluster[F]] = Map[Vector[F], Cluster[F]](),
    val joinheads: Map[Vector[F], Vector[F]] = Map[Vector[F], Vector[F]](),
    val joinlasts: Set[Vector[F]] = Set[Vector[F]]())
        extends Iterable[Cluster[F]] {

    def iterator = {
        println("starts = " + (heads.keySet -- joinlasts).toList.size)
        println("continues = " + joinlasts.size)

        println(joinheads.values.toSet.size, joinheads.values.toList.size)
        println((joinheads.values.toSet & heads.values.toSet.map((x: Cluster[F]) => x.last)).size)
        println((joinheads.values.toSet & heads.keySet).size, joinheads.values.toSet.size)

        // assert(joinheads.values.toSet.size
        //     == joinheads.values.toList.size, "Duplicate joins")
        //        
        //        assert((joinheads.values.toSet & heads.values.toSet.map((x: Cluster[F]) => x.last))
        //            == Set(), "joinheads should not join to inner chains")
        //            
        /*        assert((joinheads.values.toSet & heads.keySet)
            == joinheads.values.toSet, "joinheads should point to existent chains")
*/
        (heads.keySet -- joinlasts).toList match {
            case start :: starts => Iterator.iterate[(Option[Cluster[F]], List[Vector[F]])](
                (Some(heads(start)), starts)) {
                    case (Some(c), starts) =>
                        joinheads.get(c.last) match {
                            case Some(v) => 
                                heads.get(v) match {
                                    case Some(v) =>
                                        (Some(v), starts)
                                    case None =>
                                        println("impossible failure")
                                        starts match {
                                            case start :: starts =>
                                                println(5)
                                                (heads.get(start), starts)
                                            case List() => (None, List())
                                        }
                                }
                            case None => starts match {
                                case start :: starts =>
                                    println(1)
                                    (heads.get(start), starts)
                                case List() =>
                                    println(7)
                                    (None, List())
                            }
                        }
                } takeWhile (_._1 != None) map {
                    case (Some(c), _) => c
                }
            case List() => Iterator.empty
        }
    }

    def map = foldLeft(Map[Vector[F], Set[Vector[F]]]()) {
        case (map, vs) =>
            val set = vs.toSet
            vs.foldLeft(map) {
                case (map, v) => map + (v -> set)
            }
    }

    def +(v1: Vector[F], v2: Vector[F]): Clusters[F] = {
        (heads.get(v2), lasts.get(v1)) match {
            case (Some(c1), Some(c2)) =>
                println(1, v1, v2)
                c2.unionIfCheck(c1) match {
                    case Some(c) =>
                        new Clusters(
                            (heads - v2) + (c.head -> c),
                            (lasts - v1) + (c.last -> c),
                            joinheads, joinlasts
                        )
                    case None =>
                        new Clusters(
                            heads, lasts, joinheads + (v1 -> v2), joinlasts + v2)

                }

            case (Some(c1), None) =>
                println(2, v1, v2)
                (v1 +: c1) match {
                    case c => new Clusters(
                        (heads - v2) + (c.head -> c),
                        lasts + (c.last -> c),
                        joinheads, joinlasts
                    )
                }

            case (None, Some(c2)) =>
                println(3, v1, v2)

                (c2 :+ v2) match {
                    case c => new Clusters(
                        heads + (c.head -> c),
                        (lasts - v1) + (c.last -> c),
                        joinheads, joinlasts

                    )
                }

            case (None, None) =>
                println(4, v1, v2)

                new Cluster(v1, v2) match {
                    case c => new Clusters(
                        heads + (v1 -> c),
                        lasts + (v2 -> c),
                        joinheads, joinlasts
                    )
                }
        }
    }
}

