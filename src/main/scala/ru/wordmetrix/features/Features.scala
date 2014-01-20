package ru.wordmetrix.features
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.smartfile.SmartFile
import ru.wordmetrix.utils.CFG

object Features {

    val delimiter = "\\W+".r

    def split(s: String)(implicit cfg: CFG) = {
        for {
            (x, ys) <- (for {
                word <- delimiter.split(s)
                if word.length > cfg.wordlen
            } yield word).groupBy(x => x.toLowerCase())
            y <- Some(ys.toList.length.toDouble)
            if y > cfg.wordfreq
        } yield { x -> y }
    } toList

    case class String2Word[F, C](val map: Map[F, Int] = Map[F, Int](),
                                 val rmap: Map[Int, F] = Map[Int, F](),
                                 n: Int = 0) {
        def update(word: F): (Int, String2Word[F, C]) = {
            map.get(word) match {
                case Some(x) => (x, this)
                case None =>
                    val x = n + 1
                    (x, copy(
                        map = map + (word -> x),
                        rmap = rmap + (x -> word),
                        x))
            }
        }
        
        def encode(xs: Iterable[(F, C)])(implicit cfg: CFG) =
            xs.foldLeft(
                Map[Int, C](), this
            ) {
                    case ((map, index), (x, y)) =>
                        index.update(x) match {
                            case (n, index) => (map + (n -> y), index)
                        }
                }
    }

    def fromText(s: String,
                 index: String2Word[String, Double] = String2Word[String, Double]())(implicit cfg: CFG) =
       index.encode(split(s)) match {
        case (x,y) => (Vector(x.toList), y)
    }

    def fromText(s: String)(implicit cfg: CFG) = Vector(split(s))

    def fromText(sf: SmartFile)(implicit cfg: CFG): Vector[String] =
        fromText(sf.readLines.mkString("\n"))
}

class IndexedFeatures[F](val map : Map[F,Int], val remap : Map[Int,F]) {
    //TODO: IndexedFeatures    
} 