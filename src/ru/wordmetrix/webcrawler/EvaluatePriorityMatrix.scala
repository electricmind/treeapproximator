package ru.wordmetrix.webcrawler

import scala.actors.Actor
import WebCrawler.{ Seed, Word }
import scala.collection.mutable.PriorityQueue
import ActorDebug._
import ru.wordmetrix.vector.Vector
class EvaluatePriorityMatrix(storage: Storage,
                             sample: SampleHierarchy2PriorityBase)(implicit cfg: CFG) extends Actor
        with CFGAware {
    override val name = "Evaluate . Matrix"
    type Priority = Double
    type V = ru.wordmetrix.vector.Vector[Word]
    type Item = (Priority, Seed)
    type VItem = (Priority, V)

    var priorities = Map[Seed, (Priority, Set[Seed])]()

    var vectors = Map[Seed, (V, Set[Seed])]()

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    val dispatcher = new Dispatcher(this) {
        start
    } 

    var mode = 0

    var factor = new V(List())

    var newfactor = new V(List())

    var target = new TargetVector[String](n = cfg.targets)

    var average = new AverageVector[String]()

    var limit = 0.90

    def calculate(factor: V, vectors: Map[Seed, (V, Set[Seed])]) =
        vectors.map({
            case (seed, (vector, seeds)) => {
                val priority = vector * factor.normal
                seeds.map((_, priority))
            }
        }).flatten.groupBy(x => x._1).map({
            case (seed, ps) =>
                seed -> ps.map(_._2)
        }).map({
            case (seed, ps) =>
                seed -> ((combinepolicy(ps), priorities(seed) /*.getOrElse(seed, (0d, Set[Seed]()))*/ ._2))
        })

    def combinepolicy(priorities: Iterable[Double]) = priorities.max

    var amount = 4

    var central = new V(List())

    def enqueue(seeds: Set[Seed], seed: Seed, v: V) = debug.time("enque") {
        vectors = vectors + (seed -> (v, seeds))
        for (item <- seeds) {
            val qq = (
                item -> (
                    (combinepolicy(
                        priorities.getOrElse(
                            item,
                            (0d, Set[Seed]())
                        )._2.map(
                                x => vectors(x)._1 * factor) + v * factor
                    ),
                        priorities.getOrElse(item, (0d, Set[Seed]()))._2 + seed
                    )
                )
            )
            //            this.log("QQ %s %s",qq._1,qq._2)
            priorities = priorities + qq
        }

        if (queue.isEmpty) {
            this.log("Empty queue, beg dispatcher for webget")
            this ! dispatcher
        }

        queue.clear()
        for ((seed, (p, seeds)) <- priorities) {
            queue.enqueue((p, seed))
        }
    }

    def estimate(seed: Seed, v: V) = debug.time("estimate, average size: %s, target size: %s".format(average.vector.size,  target.average.vector.size)) {
        average = average + v
        target = target + (v, {
            this.debug("accepted %s with %s in %s", seed, v * target.average.normal, target.priority())
            storage ! seed
        })

        newfactor = target.normal - average.normal

        this.debug("direction = %s %s",
            (target.normal - average.normal) * central, factor * central)

        this.debug("limit? %s < %s",
            newfactor.normal * factor.normal, limit
        )
    }

    def act() = loop {
        react {
            case (seed: Seed) => {
                this.log("Initial seed: %s", seed)
                dispatcher ! seed
            }

            case (seeds: Set[Seed], seed: Seed, v: V) => mode = mode match {
                case 0 => {
                    //Initialization
                    val v1 = v.normal
                    this.log("Initial phase, v = %s, n = %s, seed = %s",
                        v.norm, seeds.size, seed)
                    central = v1
                    target = target + v1
                    average = average + v1

                    for (seed <- seeds) {
                        dispatcher ! seed
                    }

                    storage ! seed
                    1
                }

                case 1 => {
                    //Accumulate initial vectors for target
                    estimate(seed, v.normal)
                    this.log("Targeting phase, attitude = %s, n = %s, " +
                        "seed = %s",
                        newfactor * central, seeds.size, seed)

                    this.debug("target - average| = %s",
                        (target.normal - average.normal).norm)

                    this.debug("target*central = %s",
                        target.normal * central)

                    factor = newfactor
                    enqueue(seeds, seed, v)
                    queue.clear()

                    if (factor * central > cfg.targeting) {
                        priorities = calculate(newfactor, vectors)
                        //target = new TargetVectorCluster[String](target, n = cfg.targets)
                        2
                    } else {
                        1
                    }
                }

                case 2 => {
                    //Do work
                    this.log("Estimating phase,  attitude = %s, priority = %s, seed = %s",
                        newfactor * central, factor * v.normal, seed)
                    estimate(seed, v.normal)
                    if (newfactor.normal * factor.normal < limit) {
                        this.debug("Priorities should be recalculated")
                        priorities = calculate(newfactor, vectors)
                        factor = newfactor
                    }

                    enqueue(seeds, seed, v)
                    sample ! (seed, factor * v.normal)
                    2
                }
            }

            case dispatcher: Dispatcher => {
                this.debug("Get dispather request")
                if (!queue.isEmpty) {
                    val (p, seed) = queue.dequeue
                    val (_, seeds) = priorities(seed)
                    priorities = priorities - seed
                    vectors = vectors ++ {
                        seeds.filter(vectors contains _).map(x => x -> {
                            val (vector, seeds) = vectors(x)
                            (vector, seeds - seed)
                        })
                    }
                    this.log("Request, priority = %s for %s : %s", p,
                        seed, seeds.headOption.getOrElse("empty"))
                    dispatcher ! seed
                } else {
                    this.debug("Queue was empty")
                }
            }

            case None => {
                this ! dispatcher
            }

            case webget: WebGet => dispatcher ! webget
            case x => this.debug("!!!! Unknown message %s from %s",
                x, sender)
        }
    }
}