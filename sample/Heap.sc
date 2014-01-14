import scalaz._
import std.option._, std.list._
import ru.wordmetrix.utils.impl

object HeapTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet


    val heap = Heap.fromData(List(1))             //> heap  : scalaz.Heap[Int] = <heap>

    val heap2 = heap.insert(2)                    //> heap2  : scalaz.Heap[Int] = <heap>

    heap2.isEmpty                                 //> res0: Boolean = false

    val (a, b) = heap2.->(1)                      //> a  : scalaz.Heap[Int] = <heap>
                                                  //| b  : Int = 1
    a                                             //> res1: scalaz.Heap[Int] = <heap>
    b                                             //> res2: Int = 1

    /* implicit class HeapEx[U](heap: Heap.type) {
        def unapply[U](heap: Heap[U]) =
            if (heap.isEmpty)
                (None, heap)
            else
                (heap.minimum, heap.deleteMin)
    } */

    object Pop {
        def apply[U](x: Option[U], heap: Heap[U])(implicit o: scalaz.Order[U]) =
            x match {
                case Some(x) => heap.insert(x)
                case None    => heap

            }

        def unapply[U](heap: Heap[U]) = Some(
            if (heap.isEmpty)
                (None, heap)
            else
                (Some(heap.minimum), heap.deleteMin)
        )
    }

    Heap.fromData(List(1, 2, 3)) match {
        case Pop(x, heap) => x
    }                                             //> res3: Option[Int] = Some(1)

    val Pop(x, heape1) = heap2                    //> x  : Option[Int] = Some(1)
                                                  //| heape1  : scalaz.Heap[Int] = <heap>

    object PQ {
        import scalaz._
        import std.option._, std.list._
        def apply[U]()(implicit o: scalaz.Order[U]) = Heap.fromData(List[U]())
        def apply[U](i1: U)(implicit o: scalaz.Order[U]) = Heap.fromData(List[U](i1))
        def apply[U](i1: U, heap: Heap[U])(implicit o: scalaz.Order[U]) = heap.insert(i1)
        def unapply[U](heap: Heap[U]) =
            if (heap.isEmpty) None else Some((heap.minimum, heap.deleteMin))
    }

    object Empty {
        def unapply[U](heap: Heap[U]) =
            if (heap.isEmpty) Some(heap) else None
    }

    val PQ(i2, h1) = heap2                        //> i2  : Int = 1
                                                  //| h1  : scalaz.Heap[Int] = <heap>
    val PQ(i1, h0) = h1                           //> i1  : Int = 2
                                                  //| h0  : scalaz.Heap[Int] = <heap>
    val Empty(em) = h0                            //> em  : scalaz.Heap[Int] = <heap>

    //    val Pop1(i0, empty) = h0

}