// val input = os.pwd / "input" / "day10.txt"
// val input = os.pwd / "input" / "day10test1.txt"
// val input = os.pwd / "input" / "day10test2.txt"
val input = os.pwd / "input" / "day10test3.txt"

// Coordinates: (0, 0) in upper left, x (column) increases to the right, y (row) increases down, y/row major order

case class Point(val x: Int, val y: Int) extends Ordered[Point]:
  def compare(that: Point): Int =
    x.compare(that.x) match
      case c if c == 0 => y.compare(that.y)
      case c => c

  def +(p: Point): Point = Point(x + p.x, y + p.y)

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
      .filter(p1 => p1 >= Point(0, 0) && p1 < Point(map(p.y).size, map.size))

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

val map = os.read(input).linesIterator
  .map(_.map(s => Pipe.values.find(s == _.symbol).get).toVector)
  .toVector

given Vector[Vector[Pipe]] = map

// map.foreach(row => println(row.map(_.symbol).mkString))

val startY = map.indexWhere(_.exists(_ == Pipe.Start))
val startX = map(startY).indexOf(Pipe.Start)
println(s"start pipe ${map(startY)(startX)}")

val path = loop(Point(startX, startY)).toList

val r = path.size / 2

def isInside(path: List[Point], p: Point): Boolean =
  p match
    case q if path.contains(q) => false
    case Point(x, y) =>
      path
        .filter(q => q.y == y && q.x < x)
        .count(q => map(q.y)(q.x).connections.contains(North))
          % 2 == 1
// def isInside(l: List[Point], p: Point): Boolean =
//   if l.exists(_ == p) then
//     false
//   else
//     val count = l.map(q => (q, map(q.y)(q.x)))
//       .count(_ match
//         case (Point(x, y), e) if y == p.y && x < p.x && e == Pipe.Vertical =>
//           println(s"match (${p.x}, ${p.y}) - ($x, $y) $e")
//           true
//         case _ => false
//       )
//     count % 2 == 1

isInside(path, Point(3, 6))

var count = 0
for
  y <- 0 until map.size
  x <- 0 until map(y).size
  if isInside(path, Point(x, y))
do
  println(s"$x, $y")
  count = count + 1

count

isInside(path, Point(10, 1))

// up up  0 / 2
// dn up  1
// up dn  1
// dn dn  0 / 2
