import scala.collection.mutable

val input = os.pwd / "input" / "day8.txt"
// val input = os.pwd / "input" / "day8test1.txt"
// val input = os.pwd / "input" / "day8test2.txt"

enum Instruction(val code: Char):
  case Left extends Instruction('L')
  case Right extends Instruction('R')
import Instruction.*

case class Node(left: String, right: String)

val nodePattern = raw"(\w+)\s+=\s+\((\w+),\s*(\w+)\)\s*".r

val lines = os.read(input).linesIterator

val instructions = lines.next().map(c => Instruction.values.find(_.code == c).get)
assert(lines.next() == "")

def instructionIterator: Iterator[Instruction] =
  new Iterator[Instruction]:
    var i = instructions.iterator
    def hasNext: Boolean = true
    def next(): Instruction =
      if !i.hasNext then i = instructions.iterator
      i.next()

val nodes = lines
  .map(l =>
    l match
      case nodePattern(n, l, r) => (n, Node(l, r))
  )
  .toMap

def step(start: String, instruction: Instruction): String =
  instruction match
    case Left => nodes(start).left
    case Right => nodes(start).right

val it = instructionIterator
val steps: LazyList[String] =
  def generate(h: String): LazyList[String] = h #:: generate(step(h, it.next()))
  generate("AAA")


// AoC 2023 day 1 part 1 ------------------------------------------------------
steps.takeWhile(_ != "ZZZ").length

val start: List[String] = nodes.keys.filter(_.endsWith("A")).toList
val it2 = instructionIterator
def steps2: LazyList[List[String]] =
  def generate(h: List[String]): LazyList[List[String]] =
    val i = it2.next()
    h #:: generate(h.map(step(_, i)))
  generate(start)

// steps2.takeWhile(!_.forall(_.endsWith("Z"))).length
// steps2.take(2).foreach(println)
