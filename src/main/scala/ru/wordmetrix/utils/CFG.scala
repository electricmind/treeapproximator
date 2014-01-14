package ru.wordmetrix.utils

import java.io.File
import scala.actors.Actor

import java.net.URI
/*
 * CFG: Object that holds a set of the parameters of current session.
 */

object CFGParse {
    val rkey = """-(.+)""".r

    def apply(args: Array[String]): CFG = apply(args.toList)

    def apply(list: List[String], cfg : CFG = CFG(),
              seeds: List[URI] = List()): CFG = list match {
        
        case rkey("h") :: list =>
            println(cfg)
            scala.sys.exit
            apply(list, cfg, seeds)
            
        case rkey("path") :: path :: list =>
            apply(list, cfg.copy(path = new File(path)), seeds)

        case rkey("d") :: list =>
            apply(list, cfg.copy(isdebug = true), seeds)

        case rkey("ish2p") :: list =>
            apply(list, cfg.copy(ish2p = true), seeds)

        case rkey("servers") :: value :: list =>
            apply(list, cfg.copy(servers = value.toInt), seeds)

        case rkey("targeting") :: value :: list =>
            apply(list, cfg.copy(targeting = value.toDouble), seeds)

        case rkey("targets") :: value :: list =>
            apply(list, cfg.copy(targets = value.toInt), seeds)

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

        case seed :: list =>
           apply(list, cfg.copy( seeds = new URI(seed) :: seeds))

          
        case List() => cfg.copy(
            sampling = cfg.sampling match {
                case x if x.isAbsolute => x
                case x                 => new File(cfg.path, x.getPath)
            },
            seeds = cfg.seeds.reverse)
    }
}
/*
 *[04:02] Lynne: a tin of striped paint
 * 
*/
case class CFG(val path: File = new File("/tmp/webcrawler"), val sampling: File = new File("sampling.lst"), val isdebug: Boolean = false,
          val ish2p: Boolean = false, val servers: Int = 2,
          val targeting: Double = 0.01, val targets: Int = 9,
          val sigma: Double = 1.0, val limit: Int = 100, val cache: File = new File("/tmp/webgetcache"), val seeds: List[URI] = List()) {}


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
}

trait CFGAware {
    val name = "CFGAware"
    def debug(format: String, p: Any*)(implicit cfg: CFG) =
       ru.wordmetrix.utils.debug(this, format, p: _*)
        
    def log(format: String, p: Any*)(implicit cfg: CFG) =
       ru.wordmetrix.utils.log(this, format,  p: _*)
        
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