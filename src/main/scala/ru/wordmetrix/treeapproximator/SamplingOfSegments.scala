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
import java.io.{ File, InputStream }
import java.net.URI

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import ru.wordmetrix.smartfile.SmartFile.fromFile

object SamplingOfSegments extends App {

    override def main(args: Array[String]) {
        val (files, path) = (args match {
            case Array() =>
                printf("Enter SamplingOfSegments <uri> [<path>]\n")
                sys.exit
                (null, null)
            case Array(file)             => (List(file), ".")
            case Array(file, files @ _*) => (file :: files.toList.dropRight(1), files.last)

        }) match {
            case (files, path) => (files.map(new URI(_)), new File(path))
        }

        def write(paragraph: List[String], n: Int, name: String) = if (paragraph.length > 10)
            (path /
                s"${name}.%04d.txt".format(n))
                .write(
                    s"Page $n, ${name}\n\n"
                        + paragraph.mkString("\n")
                        + "\n"
                )

        val end = Future.sequence(files.map(file => for {
            content <- Future(file.toURL.getContent())
            lines <- content match {
                case content: InputStream =>
                    Future(io.Source.fromInputStream(content).getLines.toList)
                case _ =>
                    Future.failed(new Exception("Invalid data type"))
            }
        } yield {
            Iterator.iterate((List[String](), lines))({
                case (_, lines) => lines.splitAt(200)
            }).takeWhile(_._2.nonEmpty).map(_._1).zipWithIndex foreach {
                case (lines, n) => write(lines, n, file.getPath().split("/").toList.last)
            }
        }
        ))

        Await.ready(end, 1 minute)
    }
}