val input = os.pwd / "input" / "day3.txt"
// val input = os.pwd / "input" / "day3test.txt"

case class Point(x: Int, y: Int):
  def +(v: Int) = Point(x + v, y + v)
  def +(v: Point) = Point(x + v.x, y + v.y)
  def -(v: Int) = Point(x - v, y - v)
  def -(v: Point) = Point(x - v.x, y - v.y)

case class Rect(upperLeft: Point, lowerRight: Point):
  def intersects(r: Rect): Boolean =
    (this.upperLeft.x max r.upperLeft.x) < (this.lowerRight.x min r.lowerRight.x)
      && (this.upperLeft.y max r.upperLeft.y) < (this.lowerRight.y min r.lowerRight.y)
  def outset: Rect =
    Rect(upperLeft - 1, lowerRight + 1)

trait Part:
  val location: Rect
  val symbol: String

case class PartNumber(location: Rect, symbol: String) extends Part

case class PartSymbol(location: Rect, symbol: String) extends Part

case class PartEmpty(location: Rect, symbol: String) extends Part

val schematicSplitter = raw"(?:(\.+)|(\d+)|([^.0-9]))".r

extension (s: String)
  def parseSchematicLine(y: Int): Seq[Part] =
    var l = List[Part]()
    val m = schematicSplitter.findAllIn(s)
    while m.hasNext do
      val location = Rect(Point(m.start, y), Point(m.end, y+1))
      val symbol = Option(m.group(1)).map(PartEmpty(location, _))
        .orElse(Option(m.group(2)).map(PartNumber(location, _)))
        .orElse(Option(m.group(3)).map(PartSymbol(location, _)))
        .get
      l = symbol :: l
      m.next()
    l

val parts = os.read(input).linesIterator
  .zipWithIndex
  .map((l, i) => l.parseSchematicLine(i))
  .flatten
  .toSeq

val symbols = parts.filter(_.isInstanceOf[PartSymbol])

// AoC 2023 day 3 part 1 ------------------------------------------------------
parts
  .filter(_.isInstanceOf[PartNumber])
  .filter(p => symbols.exists(s => s.location.intersects(p.location.outset)))
  .map(_.symbol.toInt).sum

// AoC 2023 day 3 part 2 ------------------------------------------------------
symbols
  .filter(_.symbol == "*")
  .map(s => parts.filter(_.isInstanceOf[PartNumber]).filter(p => s.location.intersects(p.location.outset)))
  .filter(l => l.size == 2)
  .map(l => l(0).symbol.toInt * l(1).symbol.toInt)
  .sum
