package ru.wordmetrix.treeapproximator

import java.io.File


import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.util.matching.Regex


import ru.wordmetrix.smartfile.SmartFile.{fromFile, fromString, toFile}
import ru.wordmetrix.utils._
import ru.wordmetrix.utils.Use.anyToUse
import ru.wordmetrix.vector.Vector

class ArrangeTextDumpTree(val arrangetree: ArrangeText)(implicit cfg: CFG)
        extends ArrangeTextDump(arrangetree) {

   val tree = arrangetree.tree_aligned
    
   val path = cfg.path
   
   type Word = Int
   
   type Node = TreeApproximator.Node[Word, File]
   type Tree = TreeApproximator.Tree[Word, File]
   type Leaf = TreeApproximator.Leaf[Word, File]
   type Empty = TreeApproximator.Empty[Word, File]

    def dump(tree: Tree = tree, path: File = path): Unit = tree match {
        case node : Empty =>
            log("Tree is empty")
            
        case node: Node => {
            val stopword = "[\\W+]".r.split(path.toString).map(_.trim).toSet

            val centroid_delta_1 =
                node.child1.average.normal - node.child2.average.normal

            val path1 = path / (
                if (path.toString.length > 1000)
                    "1"
                else "1 : %s".format(
                    vector2Title(centroid_delta_1, 3, stopword)
                )
            )

            path1 / "vocabulary.txt" write (centroid_delta_1)

            dump(node.child1, path1)

            val centroid_delta_2 =
                node.child2.average.normal - node.child1.average.normal

            val path2 = path / (
                if (path.toString.length > 1000)
                    "2"
                else
                    "2 : %s".format(
                        vector2Title(centroid_delta_2, 3, stopword)
                    )
            )

            path2 / "vocabulary.txt" write (centroid_delta_2)
            
            dump(node.child2, path2)
        }

        case leaf: Leaf =>
            leaf.value.copyTo(path / leaf.value.getName().toString)
    }
}