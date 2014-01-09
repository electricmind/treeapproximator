package ru.wordmetrix.utils

import java.net.URI
import scala.util.matching.Regex.Match

object Utils {
    def uriToFilename(uri: URI) = """[/:_\-\\]""".r.replaceAllIn(
        """https?://""".r.replaceFirstIn(uri.toString, ""), x => x match {
            case Match("/") => "---"
            case Match("-") => "--"
            case Match(":") => "__"
            case Match("_") => "___"
        }) match {
            case x if x.length > 120 => x.slice(0, 120) +
            x.slice(0, 120).hashCode.toString
            case x => x
        }

    def filenameToUri(s: String) =
        new URI("http://" + """---|--|___|__""".r.replaceAllIn(
            s,
            x => x match {
                case Match("---") => "/"
                case Match("--")  => "-"
                case Match("__")  => ":"
                case Match("___") => "_"
            }
        ))
}