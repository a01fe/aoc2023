import scala.collection.mutable

val input = os.pwd / "input" / "day4.txt"
// val input = os.pwd / "input" / "day4test.txt"

extension (i: Int)
  def **(e: Int) =
    scala.math.pow(i, e).intValue

case class Card(id: Int, winning: Set[Int], have: Set[Int]):
  def matches: Int = have.count(winning.contains(_))

val cardPattern = raw"Card\s+(\d+):\s+((?:\d+\s+)*)\|\s+((?:\d+\s*)*)".r

object Card:
  def apply(s: String): Card =
    s match
      case cardPattern(c, w, h) => Card(
        c.toInt,
        w.trim().split(raw"\s+").map(_.toInt).toSet,
        h.trim().split(raw"\s+").map(_.toInt).toSet
      )

val cards = os.read(input).linesIterator
  .map(Card(_))
  .toSeq

// AoC 2023 day 4 part 1 ------------------------------------------------------
cards.map(m => 2 ** (m.matches - 1)).sum

val counts = mutable.Map[Int, Int]()

cards.foreach(c =>
  val n = counts.getOrElseUpdate(c.id, 1)
  (1 to c.matches)
    .foreach(x => counts(c.id + x) = counts.getOrElse(c.id + x, 1) + n)
)

// AoC 2023 day 4 part 2 ------------------------------------------------------
counts.values.sum
