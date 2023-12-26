import os.proc
import scala.collection.mutable.ArrayDeque

val input = os.pwd / "input" / "day15.txt"
// val input = os.pwd / "input" / "day15test.txt"

extension (s: String)
  def hash15(): Int =
    s.foldLeft(0)((h, c) => (h + c.toInt) * 17 % 256)

"HASH".hash15()

case class Lens(val label: String, val fl: Int)

var is = os.read(input).strip().split(",").toList

// AoC 2023 day 15 part 1 -----------------------------------------------------
is.map(_.hash15()).sum

val box = Vector.fill(256)(ArrayDeque.empty[Lens])

val stepPattern = raw"([a-z]+)(?:(-)|(=)(\d+))".r

def process(s: String): Unit =
  s match
    case stepPattern(l, "-", _, fl) =>
      box(l.hash15()).removeFirst(_.label == l)
    case stepPattern(l, _, "=", fl) =>
      val b = box(l.hash15())
      b.indexWhere(_.label == l) match
        case i if i == -1 => b.append(Lens(l, fl.toInt))
        case i => b.update(i, Lens(l, fl.toInt))

def fp(b: ArrayDeque[Lens], i: Int): Int =
  b.zipWithIndex
    .map((l, j) => (i + 1) * (j + 1) * l.fl)
    .sum

// AoC 2023 day 15 part 2 -----------------------------------------------------
is.foreach(process(_))
box.zipWithIndex.map((b, i) => fp(b, i)).sum
