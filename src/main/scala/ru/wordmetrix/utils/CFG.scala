package ru.wordmetrix.utils

import java.io.File

import java.net.URI
/*
 * CFG: Object that holds a set of the parameters of current session.
 */

object CFG {
    val rkey = """-(.+)""".r

    // def apply(args: Array[String]): CFG = apply(args.toList)

    object F2U extends Enumeration {
        val Simple, Dump, Map = Value
    }

    def apply(list: List[String]): CFG = apply(list, CFG(), List())

    def apply(list: List[String], cfg: CFG, // = CFG(),
              seeds: List[URI] // = List()
              ): CFG = list match {

        case rkey("h") :: list =>
            println(cfg)
            scala.sys.exit
            apply(list, cfg, seeds)

        case rkey("map") :: file :: list =>
            apply(list, cfg.copy(map = new File(file), f2u = F2U.Map), seeds)

        case rkey("f2u") :: f2u :: list =>
            apply(list, cfg.copy(f2u = f2u match {
                case "simple" => F2U.Simple
                case "dump"   => F2U.Dump
                case "map"    => F2U.Map
            }), seeds)

        case rkey("path") :: path :: list =>
            apply(list, cfg.copy(path = new File(path)), seeds)

        case rkey("d") :: list =>
            apply(list, cfg.copy(isdebug = true), seeds)

        case rkey("ish2p") :: list =>
            apply(list, cfg.copy(ish2p = true), seeds)

        case rkey("servers") :: value :: list =>
            apply(list, cfg.copy(servers = value.toInt), seeds)

        case rkey("rectify") :: value :: list =>
            apply(list, cfg.copy(rectify = value.toInt), seeds)

        case rkey("rectify_inline") :: value :: list =>
            apply(list, cfg.copy(rectify_inline = value.toInt), seeds)

        case rkey("wordlen") :: value :: list =>
            apply(list, cfg.copy(wordlen = value.toInt), seeds)

            
        case rkey("wordfreq") :: value :: list =>
            apply(list, cfg.copy(wordfreq = value.toInt), seeds)

        case rkey("targeting") :: value :: list =>
            apply(list, cfg.copy(targeting = value.toDouble), seeds)

        case rkey("prioriting") :: value :: list =>
            apply(list, cfg.copy(prioriting = value.toDouble), seeds)

        
        case rkey("targets") :: value :: list =>
            apply(list, cfg.copy(targets = value.toInt), seeds)

        case rkey("central") ::  path :: list =>
            apply(list, cfg.copy(central = Option(new File(path))), seeds)

        case rkey("sampling") :: path :: list =>
            apply(list, cfg.copy(sampling = new File(path)), seeds)

        case rkey("sigma") :: value :: list =>
            apply(list, cfg.copy(sigma = value.toDouble), seeds)

        case rkey("limit") :: value :: list =>
            apply(list, cfg.copy(limit = value.toInt), seeds)

        case rkey("cache") :: path :: list =>
            apply(list, cfg.copy(cache = new File(path)), seeds)

        case rkey(x) :: list => {
            println("Unknown key %s".format(x))
            apply(list, cfg, seeds)
        }

        case arg :: list =>
            apply(list, cfg.copy(args = arg :: cfg.args), seeds)

        case List() =>
            def absolute(x: File) = x match {
                case x if x.isAbsolute => x
                case x                 => new File(cfg.path, x.getPath)
            }
            cfg.copy(
                sampling = absolute(cfg.sampling),
                map = absolute(cfg.map),
                args = cfg.args.reverse
            )
    }
}
/* 
 *[04:02] Lynne: a tin of striped paint
 * 
*/
case class CFG(
        val path: File = new File("/tmp/webcrawler"),
        val sampling: File = new File("sampling.lst"),
        val map: File = new File("map.lst"),
        val central: Option[File] = None,
        val isdebug: Boolean = false,
        val ish2p: Boolean = false,
        val servers: Int = 2,
        val rectify :Int = 5,
        val rectify_inline :Int = 2,
        val targeting: Double = 0.01,
        val prioriting: Double = 0.9,
        val targets: Int = 9,
        val sigma: Double = 1.0,
        val limit: Int = 1000,
        val wordlen: Int = 3,
        val wordfreq: Int = 5,
        val f2u: CFG.F2U.Value = CFG.F2U.Simple,
        val cache: File = new File("/tmp/webgetcache"),
        val args: List[String] = List()) {

    lazy val seeds = args.map(x => new URI(x))
    lazy val files = args.map(x => new File(x))
    lazy val target = central getOrElse files.head
}

object debug {
    def apply(format: String, p: Any*)(implicit cfg: CFG) = {
        if (cfg.isdebug) println("  " + format.format(p: _*))
    }

    def apply(actor: CFGAware, format: String, p: Any*)(implicit cfg: CFG) = {
        if (cfg.isdebug) println("  - %20s: ".format(actor.name.slice(0, 20)) + format.format(p: _*))
    }
    def trace[B](s: String = "%s")(f: => B)(implicit cfg: CFG): B = {
        val outcome = f
        apply(s, outcome)
        outcome
    }
    def time[B](s: String)(f: => B)(implicit cfg: CFG): B = {
        val t = System.currentTimeMillis()
        val outcome = f
        apply("%s", "%s : time %d".format(s, (System.currentTimeMillis() - t) / 1))
        outcome
    }
}

object log {
    def apply(format: String, p: Any*)(implicit cfg: CFG) = {
        println({ if (cfg.isdebug) "* " else "" } + format.format(p: _*))
    }

    def apply(actor: CFGAware, format: String, p: Any*)(implicit cfg: CFG) = {
        println({ if (cfg.isdebug) "* " else "" } + "- %20s: ".format(actor.name.slice(0, 20)) + format.format(p: _*))
    }
    
    def time[B](s: String)(f: => B)(implicit cfg: CFG): B = {
        val t = System.currentTimeMillis()
        val outcome = f
        apply("%s", "%s : time %d".format(s, (System.currentTimeMillis() - t) / 1))
        outcome
    }

}

trait CFGAware {
    val name = "CFGAware"
    def debug(format: String, p: Any*)(implicit cfg: CFG) =
        ru.wordmetrix.utils.debug(this, format, p: _*)

    def log(format: String, p: Any*)(implicit cfg: CFG) =
        ru.wordmetrix.utils.log(this, format, p: _*)

    def time[B](s: String)(f: => B)(implicit cfg: CFG): B =
        ru.wordmetrix.utils.debug.time(name + " : " + s)(f)(cfg)

    def trace[B](s: String)(f: => B)(implicit cfg: CFG): B =
        ru.wordmetrix.utils.debug.trace(name + " : " + s)(f)(cfg)

}

object Use {
    implicit def anyToUse[A](a: A) = new Use(a)
}

class Use[A](a: A) {
    def use[B](f: A => B) = f(a)
}