package ru.wordmetrix.treeapproximator
import ru.wordmetrix.vector._
import ru.wordmetrix.utils.Use._
 
import Clusters._

class Cluster[F](val vector: V[Vector[F]],
                 val squares: Double = 0d) extends Iterable[Vector[F]] {
    def iterator = vector.iterator

    def this(vector: V[Vector[F]]) = this(
        vector.toVector,
         average(pairs(vector).map({ case (x, y) => (x - y).sqr }).toVector)
    )

    def this(vs: Vector[F]*) = this(vs.toVector)

    def edge = Set(vector.head) | Set(vector.last)

    override def head = vector.head
    override def last = vector.last

    val coef = 3

    def n = vector.length - 1
    def dispersion: Double = if (n < 1) 0 else squares / n

    private def factory(v: V[Vector[F]], distance: Double) = new Cluster[F](
        v, squares + distance * distance
    )

    def :+(v: Vector[F]) = factory(vector :+ v, (vector.last - v).norm)

    def +:(v: Vector[F]) = factory(v +: vector, (vector.head - v).norm)

    def check(distance: Double) = {
        n < 1 || distance * distance < dispersion * coef * coef
    }

    def unionIfCheck(that: Cluster[F]) = (this.last - that.head).norm match {
        case x => if (check(x) && that.check(x)) {
            Some(union(that, x))
        } else {
            None
        }
    }

    def union(that: Cluster[F]): Cluster[F] = union(that, (last - that.head).norm)

    def union(that: Cluster[F], distance: Double) = {
        new Cluster[F](
            vector ++ that.vector,
            squares + that.squares + distance * distance
        )
    }
}

