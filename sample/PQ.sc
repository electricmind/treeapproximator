 import scalaz._
        import std.option._, std.list._
       
object PQ {
    println("Welcome to the Scala worksheet")     //> Welcome to the Scala worksheet
    object PQ {
        import scalaz._
        import std.option._, std.list._
        def apply[U]()(implicit o: scalaz.Order[U]) = Heap.fromData(List[U]())
        def apply[U](i1: U)(implicit o: scalaz.Order[U]) = Heap.fromData(List[U](i1))
        def apply[U](x1: U, x2 : U, xs : U*)(implicit f: scalaz.Foldable[List], o: scalaz.Order[U] ) =
            xs.foldLeft(Heap.fromData(List(x1,x2))) {
               case (heap,x) => heap.insert(x)
            }
        def apply[U](i1: U, heap: Heap[U])(implicit o: scalaz.Order[U]) = heap.insert(i1)
        def unapply[U](heap: Heap[U]) =
            if (heap.isEmpty) None else Some((heap.minimum, heap.deleteMin))
    }
    
    PQ[Int]()                                     //> res0: scalaz.Heap[Int] = <heap>
    
    PQ(1) match { case PQ(x,xs) => (x,xs.toList)} //> res1: (Int, List[Int]) = (1,List())
    
    PQ(1,2) match { case PQ(x,xs) => (x,xs.toList)}
                                                  //> res2: (Int, List[Int]) = (1,List(2))
 
    PQ(1,2,3) match { case PQ(x,xs) => (x,xs.toList)}
                                                  //> res3: (Int, List[Int]) = (1,List(2, 3))
    
    PQ(1,2,3,4) match { case PQ(x,xs) => (x,xs.toList)}
                                                  //> res4: (Int, List[Int]) = (1,List(2, 3, 4))
    
    PQ(1,2,3,4,5) match { case PQ(x,xs) => (x,xs.toList)}
                                                  //> res5: (Int, List[Int]) = (1,List(2, 3, 4, 5))
    PQ[Int]                                       //> res6: scalaz.Heap[Int] = <heap>
    
}