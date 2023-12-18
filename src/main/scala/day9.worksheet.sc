val input = os.pwd / "input" / "day9.txt"
// val input = os.pwd / "input" / "day9test.txt"

def diff0(p: Int, l: List[Int]): List[Int] =
  l match
    case Nil => Nil
    case _ => (l.head - p) :: diff0(l.head, l.tail)

def diff(l: List[Int]): List[List[Int]] =
    if l.forall(_ == 0) then List(l) else l :: diff(diff0(l.head, l.tail))

val histories = os.read(input).linesIterator
  .map(_.split(raw"\s+").map(_.toInt).toList)
  .toList

// AoC 2023 day 9 part 1 ------------------------------------------------------
histories.map(diff(_))
  .map(_.foldRight(0)((r, x) => r.last + x))
  .sum

// AoC 2023 day 9 part 2 ------------------------------------------------------
histories.map(diff(_))
  .map(_.foldRight(0)((r, x) => r.head - x))
  .sum
