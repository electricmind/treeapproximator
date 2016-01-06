package ru.wordmetrix.treeapproximator

import java.io.{File, InputStream}
import java.net.URI

import ru.wordmetrix.smartfile.SmartFile.fromFile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
 * An utility that prepares a sample of short text fragments.
 *
 * This utility downloads a text from given URL's and split it on
 * strings that comply with a pattern.
 *
 * @author Elec
 * @usage SamplingOfSections [ -regexp <REGEXP> ] { <URI> }+ [<Path to store>]
 */

object SamplingOfSections extends App {
  override def main(args: Array[String]) {
    val (regexp, files, path) = (args match {

      case Array("-regexp", regexp, args@_*) => (regexp.r, args)

      case Array("-mark", mark, args@_*) =>
        (s".+$mark.+".r, args)

      case Array(args@_*) => ("^\\s*$".r, args)

    }) match {
      case (regexp, Seq(file, files@_*)) => (regexp,
        (file :: files.toList.dropRight(1)).map(new URI(_)),
        files.lastOption match {
          case Some(x) => new File(x)
          case None => new File(".")
        })
      case _ =>
        printf("Enter "
          + "SamplingOfParagraphs [-mark <Mark> | -regexp <Regexp>]"
          + " { <uri> }+ [ <path> ]\n")
        sys.exit
        (null, null, null)
    }

    def write(paragraph: List[String], n: Int, name: String) =
      if (paragraph.length > 10)
        (path /
          s"${name}.%04d.txt".format(n))
          .write(s"$n : ${name} : "
          + paragraph.mkString("\n") + "\n")

    def output(lines: List[String], n: Int, name: String): Unit = if (n < 10000)
      lines match {
        case paragraph@line :: List() =>
          write(paragraph, n, name)

        case line :: lines =>
          println(line)
          lines.span(regexp.findFirstMatchIn(_).isEmpty) match {
            case (List(), List()) =>

            case (paragraph@List(_ @ _*), lines) =>
          write (line :: paragraph, n, name)
          output (lines, n + 1, name)
          }

      }

    val end = Future.sequence(files.map { file =>
      for {
        content <- Future(file.toURL.getContent())
        lines <- content match {
          case content: InputStream =>
            Future(io.Source.fromInputStream(content).getLines.toList)
          case _ =>
            Future.failed(new Exception("Invalid data type"))
        }
      } yield {
        output(lines, 1, file.getPath().split("/").toList.last)
      }
    })

    Await.ready(end, 1 minute)
  }
}