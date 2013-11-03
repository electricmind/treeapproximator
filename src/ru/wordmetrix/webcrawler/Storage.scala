package ru.wordmetrix.webcrawler

import scala.actors.Actor
import Use._
import ActorDebug._
import ru.wordmetrix.smartfile.SmartFile._

class Storage()(implicit val cfg: CFG) extends Actor with CFGAware {
    override val name = "Storage"
    var n = 1
    def seedToFilename(seed: WebCrawler.Seed) = """[/:\\]""".r.replaceAllIn("""https?://""".r.replaceFirstIn(seed.toString, ""), "-") match {
        case x if x.length > 120 => x.slice(0, 120) +
            x.slice(0, 120).hashCode.toString
        case x => x
    }

    def act() = loop {
        react {
            case seed: WebCrawler.Seed => {
                this.log("Datum %s seemed significant", seed)
                seedToFilename(seed) use {
                  name => cfg.path / "tmp" / name copyTo cfg.path / name
                }
            }

            case (seed: WebCrawler.Seed, intel: WebCrawler.Intel) => {
                this.log("%04d - Datum %s has come", n, seed)
                cfg.path / "tmp" / seedToFilename(seed) write(intel)
                n += 1
                if (n > cfg.limit) {
                    System.exit(0)
                }
            }

            case x => log("Store got something strange: %s", x)
        }
    }

}
