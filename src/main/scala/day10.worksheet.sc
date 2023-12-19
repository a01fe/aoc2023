val input = os.pwd / "input" / "day10.txt"
// val input = os.pwd / "input" / "day10test1.txt"
// val input = os.pwd / "input" / "day10test2.txt"
// val input = os.pwd / "input" / "day10test3.txt"
// val input = os.pwd / "input" / "day10test4.txt"
// val input = os.pwd / "input" / "day10test5.txt"

// Coordinates: (0, 0) in upper left, x (column) increases to the right, y (row) increases down, y/row major order

case class Point(val x: Int, val y: Int) extends Ordered[Point]:
  def compare(that: Point): Int =
    x.compare(that.x) match
      case c if c == 0 => y.compare(that.y)
      case c => c

  def +(p: Point): Point = Point(x + p.x, y + p.y)
  def -(p: Point): Point = Point(x - p.x, y - p.y)

enum Connection(val delta: Point):
  case North extends Connection(Point(0, -1))
  case South extends Connection(Point(0, 1))
  case East extends Connection(Point(1, 0))
  case West extends Connection(Point(-1, 0))
import Connection.*

enum Pipe(val symbol: Char, val connections: List[Connection]):
  case Vertical extends Pipe('|', List(North, South))
  case Horizontal extends Pipe('-', List(East, West))
  case BendNE extends Pipe('L', List(North, East))
  case BendNW extends Pipe('J', List(North, West))
  case BendSE extends Pipe('F', List(South, East))
  case BendSW extends Pipe('7', List(South, West))
  case Ground extends Pipe('.', List())
  case Start extends Pipe('S', List(North, South, East, West))
  def steps(p: Point)(using map: Vector[Vector[Pipe]]): List[Point] =
    connections
      .map(p + _.delta)
      .filter(p1 => p1.x >= 0 && p1.y >= 0 && p1.x < map(p.y).size && p1.y < map.size)

object Pipe:
  def apply(d0: Point, d1: Point): Pipe =
    Pipe.values.find(p => p.connections.map(_.delta).contains(d0) && p.connections.map(_.delta).contains(d1)).get

  // Return coordinates of adjacent pipes we can step to
def steps(p: Point)(using map: Vector[Vector[Pipe]]): List[Point] =
  map(p.y)(p.x).steps(p)
    .filter(p1 => map(p1.y)(p1.x).steps(p1).exists(_ == p))

def loop(p0: Point)(using map: Vector[Vector[Pipe]]): Seq[Point] =
  var path = p0 :: Nil
  var pn = steps(p0).headOption
  while pn.map(_ != p0).getOrElse(false) do
    path = pn.get :: path
    pn = steps(pn.get).filter(_ != path.tail.head).headOption
  path.reverse

var map = os.read(input).linesIterator
  .map(_.map(s => Pipe.values.find(s == _.symbol).get).toVector)
  .toVector

given Vector[Vector[Pipe]] = map

val startY = map.indexWhere(_.exists(_ == Pipe.Start))
val startX = map(startY).indexOf(Pipe.Start)
val path = loop(Point(startX, startY)).toList

// AoC 2023 day 10 part 1 ------------------------------------------------------
val r = path.size / 2

// Replace start pipe with pipe that fits, isInside depends on this
map = map.updated(startY, map(startY).updated(startX, Pipe(path(1) - path(0), path.last - path(0))))

def isInside(path: List[Point], p: Point): Boolean =
  p match
    case q if path.contains(q) => false
    case Point(x, y) =>
      path
        .filter(q => q.y == y && q.x < x)
        .count(q => map(q.y)(q.x).connections.contains(North))
          % 2 == 1

var count = 0
for
  y <- 0 until map.size
  x <- 0 until map(y).size
  if isInside(path, Point(x, y))
do
  count = count + 1

// AoC 2023 day 10 part 2 ------------------------------------------------------
count
