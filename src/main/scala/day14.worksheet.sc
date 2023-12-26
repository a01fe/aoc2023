val input = os.pwd / "input" / "day14.txt"
// val input = os.pwd / "input" / "day14test.txt"

enum Ground(val symbol: Char):
  case Empty extends Ground('.')
  case Round extends Ground('O')
  case Square extends Ground('#')
import Ground.*

object Ground:
  def apply(symbol: Char): Ground =
    Ground.values.find(_.symbol == symbol).get

val map = os.read(input).linesIterator
  .map(_.map(Ground(_)).toVector)
  .toVector

def totalLoad(m: Vector[Vector[Ground]]): Int =
  val rows = m.length
  println(rows)
  m.zipWithIndex
    .tapEach(println)
    .map((r, i) => (rows - i) * r.count(_ == Round))
    .tapEach(println)
    .sum

def slide(s: List[Ground], slides: List[Ground]): List[Ground] =
  s match
    case Nil => slides
    case List(Square, _*) => slides ::: Square :: tilt(s.tail)
    case List(Round, _*) => Round :: slide(s.tail, slides)
    case List(Empty, _*) => slide(s.tail, Empty :: slides)

def tilt(s: List[Ground]): List[Ground] =
  s match
    case Nil => Nil
    case List(Round, _*) => Round :: tilt(s.tail)
    case List(Square, _*) => Square :: tilt(s.tail)
    case List(Empty, _*) => slide(s.tail, List(Empty))

def printMap(m: Vector[Vector[Ground]]): Unit =
  m.foreach(r => println(r.map(g => g.symbol).mkString))

totalLoad(map)

printMap(map)

val flippedTiltedMap = (0 until map.head.length)
  .map(j => map.map(_(j)))
  .map(c => tilt(c.toList))

val tiltedMap = (0 until flippedTiltedMap.head.length)
  .map(j => flippedTiltedMap.map(_(j)).toVector)
  .toVector

printMap(tiltedMap)

// AoC 2023 day 14 part 1 -----------------------------------------------------
totalLoad(tiltedMap)
