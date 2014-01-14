package ru.wordmetrix.webcrawler

import java.net.URI
import akka.testkit.TestProbe
import akka.actor.Props
import akka.actor.Actor
import akka.testkit.TestProbe
import akka.actor.ActorRef
import ru.wordmetrix.utils.Html2Ascii

trait Tools {
    def uri(n: Int = 0) = new URI(s"http://example.org/${n}")

    def xml(n: Int) = <html><body>
                                <a href={ uri(n).toString() }>
                                    Test Test Test Test Test
                                </a>
                                <a href={ uri(n + 1).toString() }>
                                    Test Test Test Test Test
                                </a>
                                <a href={ uri(n + 2).toString() }>
                                    Test Test Test Test Test
                                </a>
                            </body></html>

    def text(n: Int): String = Html2Ascii(xml(n) \\ "body").wrap()

    def TestActor(actor: TestProbe)(implicit system: akka.actor.ActorSystem): (TestProbe, Props) = {
        (actor, Props(new Actor {
            def receive = { case msg => actor.ref forward msg }
        }))
    }
    def TestActor()(implicit system: akka.actor.ActorSystem): (TestProbe, Props) = {
        TestActor(TestProbe())
    }

    def testParent(prop: => Props, testActor: ActorRef, name: String = "")(implicit system: akka.actor.ActorSystem) =
        system.actorOf(Props(new Actor {
            val child = context.actorOf(prop, name)
            def receive = {
                case x if sender == child => testActor forward x
                case x                    => child forward x
            }
        }))

}