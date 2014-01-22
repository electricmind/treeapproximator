package ru.wordmetrix.treeapproximator

import java.io.{ File, InputStream }
import java.net.URI
import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }
import ru.wordmetrix.smartfile.SmartFile.fromFile
import scala.concurrent.Await
import scala.concurrent._
import scala.concurrent.duration._

object SamplingOfParagraphs extends App {
    override def main(args: Array[String]) {
        val (regexp, file, path) = (args match {
            case Array("-regexp", regexp, file) => (regexp.r, file, ".")

            case Array("-mark", mark, file) =>
                (s".+$mark.+".r, file, ".")

            case Array(file)                          => ("^\\s+$".r, file, ".")

            case Array("-regexp", regexp, file, path) => (regexp.r, file, path)

            case Array("-mark", mark, file, path) =>
                (s".+$mark.+".r, file, path)

            case Array(file, path) => ("^\\s+$".r, file, path)

            case _ =>
                printf("Enter "
                    + "SamplingOfParagraphs [-mark <Mark>] <uri> [<path>]\n")
                sys.exit
                (null, null, null)

        }) match {
            case (regexp, file, path) => (regexp, new URI(file), new File(path))
        }

        def write(paragraph: List[String], n: Int) = if (paragraph.length > 10)
            (path /
                s"${file.getPath().split("/").toList.last}.%04d.txt".format(n))
                .write(s"$n : ${file.getPath().split("/").toList.last} : "
                    + paragraph.mkString("\n") + "\n")

        def output(lines: List[String], n: Int): Unit = if (n < 10000)
            lines match {
                case paragraph @ line :: List() => write(paragraph, n)

                case line :: lines =>
                    println(line)
                    lines.span(regexp.findFirstMatchIn(_).isEmpty) match {
                        case (List(), List()) =>

                        case (paragraph @ List(_@ _*), lines) =>
                            write(line :: paragraph, n)
                            output(lines, n + 1)
                    }

            }

        val end = for {
            content <- Future(file.toURL.getContent())
            lines <- content match {
                case content: InputStream =>
                    Future(io.Source.fromInputStream(content).getLines.toList)
                case _ =>
                    Future.failed(new Exception("Invalid data type"))
            }
        } yield {
            output(lines, 1)
        }

        Await.ready(end, 1 minute)
    }
}