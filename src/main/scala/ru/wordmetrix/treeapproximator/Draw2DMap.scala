package ru.wordmetrix.treeapproximator

import java.awt.{BasicStroke, Color, Graphics2D, Point}
import java.lang.Math.{max, sqrt}

import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.swing.{Dimension, MainFrame, Panel, SimpleSwingApplication}
import scala.swing.event.{KeyPressed, KeyReleased, KeyTyped, MouseClicked, MouseMoved}
import scala.swing.event.Key.{Down, Enter, Escape, Left, PageDown, PageUp, Right, Space, Up}
import scala.util.Random.nextGaussian

import ru.wordmetrix.smartfile.SmartFile.fromString
import ru.wordmetrix.treeapproximator.TreeApproximator.{Leaf, Node, Tree}
import ru.wordmetrix.utils.{CFG, debug}
import ru.wordmetrix.vector.Vector

// TODO: try paint all clusters into different tinge.

object Draw2DMap extends SimpleSwingApplication {
    implicit val cfg = CFG(isdebug=true)

    val colors = Iterator.iterate(List(
        Color.red, Color.orange, Color.yellow,
        Color.green, Color.blue, Color.magenta))({
        case color :: colors => colors :+ color
    }).map(_.head).toStream

    var args = scala.collection.Iterator[String]()

    override def startup(args: Array[String]) {
        this.args = (0 until args.size) map { x => args(x) } toIterator;
        super.startup(args)
    }
    type Value = (Vector[Int], Set[Vector[Int]], Int)

    def randomVector(d: Double, dimension: Int, av: Vector[Int]) = {
        implicit def accuracy = cfg.accuracy

        ((3 to dimension).map(
            dim => Vector(dim -> Math.abs(nextGaussian * d))
        ).fold(Vector(
            1 -> (nextGaussian * d),
            2 -> (nextGaussian * d)
        ))(_ + _) + av).normal
    }
    def generate2tree1(cloud: Iterable[(Vector[Int], Int)], d: Double, n: Int, dimension: Int)(implicit  accuracy: Double) =
        cloud.foldLeft(TreeApproximator[Int, Value]())({
            case (tree, (key, cid)) =>
                tree + (key, (key, Set(), cid)) rectify(2)
        })

    def top = new MainFrame {
        title = "Convert Celsius / Fahrenheit"
        size = new Dimension(640, 480)
        implicit def accuracy = 0.0001d
        var n = 50
        var dispersy = 0.5
        var dimension = 3
        var showalign = false
        var cluster1: Iterable[(Vector[Int], Value)] = tree
        var cluster2: Iterable[(Vector[Int], Value)] = Set()

        val cloud = scala.collection.Iterator.continually[Iterable[(Vector[Int], Int)]] {
            val nc = 4
            if (args.hasNext) {
                val data = (args.next.readLines map {
                    case x => x.split(" ") match {
                        case Array(sx, sy) => Some((sx.toDouble, sy.toDouble))
                        case _             => None
                    }
                } flatten) toStream

                val maxx = data.map(_._1).max
                val maxy = data.map(_._2).max
                val minx = data.map(_._1).min
                val miny = data.map(_._2).min

                data map {
                    case (x, y) =>
                        val xx = 2 * (x - minx) / (maxx - minx) - 1
                        val yy = 2 * (y - miny) / (maxy - miny) - 1
                        val zz = sqrt(max(0,1 - xx * xx - yy * yy))
                        (Vector(1 -> -xx, 2 -> -yy, 3 -> zz).normal, 0)
                }

            } else {

                val centroids = (1 to nc).map(
                    x => randomVector(100d, dimension, Vector[Int]())).toArray

                (1 until n)
                    .map(_ % nc)
                    .map(x => (randomVector(dispersy, dimension, centroids(x)), x))
            }
        }

        var tree = generate2tree1(cloud.next, dispersy, n, dimension)
        var nearest = tree
        var clusters = Clusters(tree)

        def regenerate() = {
            tree = generate2tree1(cloud.next, dispersy, n, dimension)
            this.repaint()
        }

        reactions += {
            case KeyPressed(_, Down, _, _) => {
                n = Math.max(n / 2, 4)
                regenerate()
            }

            case KeyPressed(_, Left, _, _) => {
                dispersy = Math.max(dispersy / 2d, 0.01)
                regenerate()
            }
            case KeyPressed(_, PageUp, _, _) => {
                dimension = Math.min(dimension + 1, 5)
                regenerate()
            }
            case KeyPressed(_, PageDown, _, _) => {
                dimension = Math.max(dimension - 1, 2)
                regenerate()
            }

            case KeyPressed(_, Right, _, _) => {
                dispersy = Math.min(dispersy * 2d, 1000.0)
                regenerate()
            }

            case KeyPressed(_, Enter, _, _) => {
                showalign = !showalign
                repaint()
            }

            case MouseMoved(_, p, _) =>
                val x = px(p)
                val y = py(p)
                val z = Math.sqrt(Math.max(0, 1 - x * x - y * y))
                val v = Vector(1 -> x, 2 -> y, 3 -> z)
                cluster1 = tree.cluster(v)
                nearest = tree.leaf(v)
                cluster2 = 
                    (debug.time("nearest") { tree(v) }) match {
                    case (n,c,i) =>
                            c.map(x => (x, (x, c, i)))
                }
                this.repaint()

            case KeyReleased(_, Space, 128, _) => {
                tree = debug.time("align") {
                    tree.align(Vector(1 -> 1.0))._1
                }

                clusters = debug.time("tree size: n=%s".format(tree.n)) {
                    Clusters(tree)
                }

                val map = debug.time("clusters: n=%s".format(clusters.size)) {
                    clusters.map
                }

                tree = debug.time("bind tree, map.size= " + map.size) {
                    tree.bind({
                        case (v, _, id) =>
                            (v, map(v), id)
                    })
                }
                
//                tree.bind(clusters.map)
                
                repaint()
            }

            case KeyPressed(_, Up, _, _) => {
                n = Math.min(n * 2, 1000)
                regenerate()
            }

            case KeyPressed(_, Escape, _, _) => {
                regenerate()
            }
            case _: MouseClicked | KeyReleased(_, Space, _, _) =>
                tree = debug.time("rectify") {
                    tree.rectify(n / 4)
                }
                println("rectify")
                this.repaint()

            //            case x              => println(x)
            case k: KeyPressed  => println(k)
            case k: KeyTyped    => println(k)
            case k: KeyReleased => println(k)

        }

        def xp(x: Double) = (x / 4 * size.width + size.width / 2).toInt
        def yp(y: Double) = (y / 4 * size.height + size.height / 2).toInt

        def px(p: Point) = ((p.x - size.width / 2d) * 4d / size.width)
        def py(p: Point) = ((p.y - size.height / 2d) * 4d / size.height)

        def point(node: Tree[Int, Value]): (Int, Int) = point(node.average, node.n)
        def point(vector: Vector[Int], n: Int): (Int, Int) = point(vector / n)
        def point(vector: Vector[Int]) =
            vector.toMap.withDefaultValue(0.0) match {
                case v => (xp(v(1)), yp(v(2)))
            }

        val draw = new Panel {
            background = Color.white
            maximize()

            def drawNode(tree: Tree[Int, Value])(implicit g: Graphics2D): (Int, Int, Int) = {
                val (x, y) = point(tree)

                val level = tree match {
                    case node: Node[Int, Value] => {
                        g.setStroke(new BasicStroke(1))
                        g.drawOval(x - 3, y - 3, 5, 5)

                        g.setStroke(new BasicStroke());
                        (drawNode(node.child1) match {
                            case (x1, y1, level) =>
                                g.setColor(Color.red);
                                g.setStroke(new BasicStroke(level));
                                g.drawLine(x, y, x1, y1);
                                level
                        }) + (
                            drawNode(node.child2) match {
                                case (x2, y2, level) =>
                                    g.setColor(Color.blue);
                                    g.setStroke(new BasicStroke(level));
                                    g.drawLine(x, y, x2, y2);
                                    level
                            })
                    }
                    case leaf: Leaf[Int, Value] =>
                        g.setStroke(new BasicStroke(1))
                        val r = (3 * (tree.average / tree.n).toMap.withDefaultValue(0.0)(3)).toInt + 5
                        g.drawOval(x - r, y - r, r * 2 + 1, r * 2 + 1)
                        //                        g.drawString(leaf.value.toString, xp(x * 1.2), yp(y * 1.2))
                        1
                }

                (x, y, 1)
            }

            override protected def paintComponent(g: Graphics2D) {
                super.paintComponent(g)

                g.setColor(Color.black)
                g.drawString("Energy:   %4.3f".format(tree.energy2), 10, 10)
                g.drawString("Dispersy: %4.3f".format(dispersy), 10, 40)
                g.drawString("Amount:   %4d".format(n), 10, 70)
                g.drawString("Dimension:   %1d".format(dimension), 10, 100)
                g.drawString("""Press:""", 10, 140)
                g.drawString(""" - Space - to rectify""", 10, 170)
                g.drawString(""" - Control+Space - to align""", 10, 200)
                g.drawString(""" - Enter - to toggle aligned view""", 10, 230)
                g.drawString(""" - Up - to increase amount of points""", 10, 260)
                g.drawString(""" - Down - to decrease amount of points""", 10, 290)
                g.drawString(""" - Left - to increase dispersy""", 10, 310)
                g.drawString(""" - Right - to decrease dispersy""", 10, 340)
                g.drawString(""" - Escape - to restart""", 10, 370)
                val icolors = colors.toIterator
                showalign match {
                    case true => tree.map({
                        case (average, _) => (average, point(average))
                    }).sliding(2).foldLeft((icolors.next, 0d, List[Double]()))({
                        case ((color, distance, l), List((v1, (x1, y1)), (v2, (x2, y2)))) => {
                            val d = (v2 - v1).norm

                            val (c, da, l1) = if (l.length < 3) {
                                g.setColor(color)
                                (color, d + distance, d :: l)
                            } else {
                                val av = l.reduce(_ + _) / l.length
                                val sigma = Math.pow(l.map(x => (x - av) * (x - av)).reduce(_ + _) / l.length, 0.5)
                                if (d - av < sigma * 2) {
                                    println("save " + d + " " + av + " " + sigma)
                                    g.setColor(color)
                                    (color, d + distance, d :: l)
                                } else {
                                    println("new  " + d + " " + av + " " + sigma
                                    )
                                    g.setColor(Color.black)
                                    (icolors.next, d, List())
                                }
                            }
                            g.setStroke(new BasicStroke(1))
                            g.drawLine(x1, y1, x2, y2)
                            val r1 = (3 * (v1.normal).toMap.withDefaultValue(0.0)(3)).toInt + 5
                            g.drawOval(x1 - r1, y1 - r1, r1 * 2 + 1, r1 * 2 + 1)

                            val r2 = (3 * (v1.normal).toMap.withDefaultValue(0.0)(3)).toInt + 5
                            //g.drawOval(x1 - 5, y1 - 5, 11, 11)
                            //                            g.drawOval(x2 - 5, y2 - 5, 11, 11)
                            g.drawOval(x1 - r2, y1 - r2, r2 * 2 + 1, r2 * 2 + 1)

                            (c, da, l1)
                        }
                    })
                    case false => {
                        drawNode(tree)(g)

                        def drawTree(node: Tree[Int, Value], color: Color) = {
                            drawPoint(point(node), color)
                        }

                        def drawPoint(p: (Int, Int), color: Color) = p match {
                            case (x, y) =>

                                g.setColor(color)
                                g.setStroke(new BasicStroke(4))

                                g.drawOval(x - 3, y - 3, 7, 7)

                        }
                        //drawTree(cluster1, Color.green)
                        cluster2.foreach {
                            case (v, _) => drawPoint(point(v), Color.cyan)
                        }
                        drawTree(nearest, Color.red)
                    }
                }
            }
        }
        listenTo(draw.mouse.clicks, draw.mouse.moves, draw.keys)
        draw.focusable = true
        contents = draw

    }
}
