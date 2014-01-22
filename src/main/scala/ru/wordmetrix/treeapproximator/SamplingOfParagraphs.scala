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
        val (file, path) = (args match {
            case Array() =>
                printf("Enter SamplingOfParagraphs <uri> [<path>]\n")
                sys.exit
                (null, null)
            case Array(file)       => (file, ".")
            case Array(file, path) => (file, path)

        }) match {
            case (file, path) => (new URI(file), new File(path))
        }

        def write(paragraph: List[String], n: Int) = if (paragraph.length > 10)
            (path /
                s"${file.getPath().split("/").toList.last}.%03d.txt".format(n))
                .write(s"page $n\n" + paragraph.mkString("\n") + "\n")

        def output(lines: List[String], n: Int): Unit = if (n < 10000) lines match {
            case List() =>

            case list =>
                list.span(!_.trim.isEmpty()) match {

                    case (List(), List()) =>

                    case (paragraph @ List(_@ _*), _ :: lines) =>
                        write(paragraph, n)
                        output(lines, n + 1)

                    case (paragraph @ List(_@ _*), List()) =>
                        write(paragraph, n)
                }
        }

        val end = for {
            
             content <- Future(file.toURL.getContent())
             lines <- content match {
                 case content : InputStream => Future(io.Source.fromInputStream(content).getLines.toList)
                 case _ =>                 Future.failed(new Exception("Invalid data type"))

             }

 /*           case Success(content) =>
                println(1)
                Future.failed(new Exception("Invalid data type"))

            case f @ Failure(t) =>
                println(2)
                
                Future.failed(t)*/
        } yield {
             output(lines, 1)
        }

        Await.ready(end, 1 minute)
    }
}