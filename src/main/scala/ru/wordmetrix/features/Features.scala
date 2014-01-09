package ru.wordmetrix.features
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.smartfile.SmartFile


object Features {
    class CFG (
        val frequency : Int = 5,
        val length : Int = 3
    )
    implicit val cfg = new CFG()
    
    val delimiter = "\\W+".r
    
    def fromText(s : String)(implicit cfg : CFG) = Vector[String](
        delimiter.split(s).groupBy(x => x.toLowerCase())
            .map({ case (x, y) => (x, y.length.toDouble) })
            .filter(_._2 > cfg.frequency)
            .filter(_._1.length > cfg.length)
            .toList)
            
    def fromText(sf : SmartFile)(implicit cfg : CFG) : Vector[String] = 
        fromText(sf.readLines.mkString("\n"))
}

class IndexedFeatures[F](val map : Map[F,Int], val remap : Map[Int,F]) {
    //TODO: IndexedFeatures    
} 