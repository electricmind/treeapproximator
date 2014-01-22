package ru.wordmetrix.treeapproximator
/**
 * An utility that prepares a sample of short text fragments.
 * 
 * This utility downloads a text from given URL and split it on 
 * slices 100 lines long.
 * 
 * @author Elec
 * @usage SamplingOfSegments <URI> [<Path to store>]
 */
import java.io.{File, InputStream}
import java.net.URI

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import ru.wordmetrix.smartfile.SmartFile.fromFile

object SamplingOfSegments extends App {

    override def main(args: Array[String]) {
        val (file, path) = (args match {
            case Array() =>
                printf("Enter SamplingOfSegments <uri> [<path>]\n")
                sys.exit
                (null, null)
            case Array(file)       => (file, ".")
            case Array(file, path) => (file, path)

        }) match {
            case (file, path) => (new URI(file), new File(path))
        }

        def write(paragraph: List[String], n: Int) = if (paragraph.length > 10)
            (path /
                s"${file.getPath().split("/").toList.last}.%04d.txt".format(n))
                .write(
                    s"Page $n, ${file.getPath().split("/").toList.last}\n\n"
                        + paragraph.mkString("\n")
                        + "\n"
                )

        val end = for {
            content <- Future(file.toURL.getContent())
            lines <- content match {
                case content: InputStream => 
                    Future(io.Source.fromInputStream(content).getLines.toList)
                case _                    => 
                    Future.failed(new Exception("Invalid data type"))
            }
        } yield {
            Iterator.iterate((List[String](),lines))({
                case (_,lines) => lines.splitAt(200)
        }).takeWhile(_._2.nonEmpty).map(_._1).zipWithIndex foreach {
                case (lines, n) => write(lines, n)
            }
        }

        Await.ready(end, 1 minute)
    }
}