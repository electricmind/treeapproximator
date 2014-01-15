

import scala.collection.immutable.TreeMap
import scala.collection.SortedMap

object SortedMapPQ {
    println("Welcome to the Scala worksheet")     //> Welcome to the Scala worksheet

    SortedMap(1 -> 2)                             //> res0: scala.collection.SortedMap[Int,Int] = Map(1 -> 2)

    object PQ {
    
        def apply[U]()(implicit o: Ordering[U]) = SortedMap[U, U]()
        def apply[U](i1: U)(implicit o: Ordering[U]) = SortedMap[U, U](i1 -> i1)
        def apply[U](x1: U, x2: U, xs: U*)(implicit o: Ordering[U]) =
            xs.foldLeft(SortedMap[U, U](x1 -> x1, x2 -> x2)) {
                case (map, x) => map + (x -> x)
            }
        def apply[U](i1: U, map: SortedMap[U, U])(implicit o: Ordering[U]) = map + (i1 -> i1)
        def unapply[U](map: SortedMap[U, U]) =
        map.headOption match {
            case None => None
            case Some((x,_)) => Some((x, map.tail))
        }
        
        implicit class PQEx[U](map : SortedMap[U,U]) {
            def insert(x : U) = map + (x->x)
        }
    }

    import PQ.PQEx
    
    PQ[Int]()                                     //> res1: scala.collection.SortedMap[Int,Int] = Map()

    PQ(1) match { case PQ(x, xs) => (x, xs.toList) }
                                                  //> res2: (Int, List[(Int, Int)]) = (1,List())
    PQ(1).insert(2)                               //> res3: scala.collection.SortedMap[Int,Int] = Map(1 -> 1, 2 -> 2)

    import scalaz._
    import std.option._, std.list._

    PQ(1, 2) match { case PQ(x, xs) => (x, xs.toList) }
                                                  //> res4: (Int, List[(Int, Int)]) = (1,List((2,2)))

    PQ(1, 2, 3) match { case PQ(x, xs) => (x, xs.toList) }
                                                  //> res5: (Int, List[(Int, Int)]) = (1,List((2,2), (3,3)))

    PQ(1, 2, 3, 4) match { case PQ(x, xs) => (x, xs.toList) }
                                                  //> res6: (Int, List[(Int, Int)]) = (1,List((2,2), (3,3), (4,4)))

    PQ(2, 1, 3, 4, 5) match { case PQ(x, xs) => (x, xs.toList) }
                                                  //> res7: (Int, List[(Int, Int)]) = (1,List((2,2), (3,3), (4,4), (5,5)))

    PQ[Int]                                       //> res8: scala.collection.SortedMap[Int,Int] = Map()
}