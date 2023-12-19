import scala.math.abs

val input = os.pwd / "input" / "day11.txt"
// val input = os.pwd / "input" / "day11test.txt"

def distance(p1: (Long, Long), p2: (Long, Long)): Long=
  abs(p1(0) - p2(0)) + abs(p1(1) - p2(1))

val galaxies = os.read(input).linesIterator
  .zipWithIndex
  .map((l, y) => l.zipWithIndex.collect(_ match
    case ('#', x) => (x.toLong, y.toLong)
  ))
  .flatten
  .toVector

def expand(g: Vector[(Long, Long)], factor: Long): Vector[(Long, Long)] =
  // Expand empty columns (same x)
  val g0 = (0L to g.map(_(0)).max)
    .map(x => (x, g.filter(_(0) == x)))
    .foldLeft((0L, Vector[(Long, Long)]()))((a, r) =>
      val x = r match
        case (x0, Vector[(Long, Long)]()) => a(0) + factor
        case _ => a(0) + 1
      (x, a(1) ++ r(1).map((x, y) => (a(0), y)))
    )(1)
  /// Expand empty rows (same y)
  (0L to g.map(_(1)).max)
    .map(y => (y, g0.filter(_(1) == y)))
    .foldLeft((0L, Vector[(Long, Long)]()))((a, r) =>
      val y = r match
        case (y0, Vector[(Long, Long)]()) => a(0) + factor
        case _ => a(0) + 1
      (y, a(1) ++ r(1).map((x, y) => (x, a(0))))
    )(1)

def sumDistances(g: Vector[(Long, Long)]): Long =
  g.combinations(2)
    .map(c => distance(c(0), c(1)))
    .sum

sumDistances(expand(galaxies, 2))

sumDistances(expand(galaxies, 10))
sumDistances(expand(galaxies, 100))

sumDistances(expand(galaxies, 1_000_000))

