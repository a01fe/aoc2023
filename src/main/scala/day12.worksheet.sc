// val input = os.pwd / "input" / "day12.txt"
val input = os.pwd / "input" / "day12test.txt"

enum Spring(val symbol: Char):
  case Operational extends Spring('.')
  case Damaged extends Spring('#')
  case Unknown extends Spring('?')
import Spring.*

object Spring:
  def apply(symbol: Char): Spring =
    Spring.values.find(_.symbol == symbol).get

case class Row(val springs: List[Spring], damagedGroups: List[Int])

val rowPattern = raw"([.#?]+)\s+(\d+(?:,\s*\d+)*)".r

val springs = os.read(input).linesIterator
  .map(_ match
    case rowPattern(s, g) => Row(s.map(Spring(_)).toList, g.split(",\\s*").map(_.toInt).toList)
  )
  .toList

def evaluateDamagedArrangement(springs: List[Spring], damagedLeft: Int, damagedGroups: List[Int]): Option[List[List[Spring]]] =
  println(s"evaluateDamagedArrangement($springs, $damagedLeft, $damagedGroups)")
  if damagedLeft == 0 then
    springs match
      case Nil if damagedGroups.isEmpty => Some(List(List()))
      case Nil => None
      case List(Operational, _*) | List(Unknown, _*) => evaluateArrangement(springs.tail, damagedGroups).map(_.map(Operational :: _))
      case List(Damaged, _*) => None
  else
    springs match
      case Nil => None
      case List(Operational, _*) => None
      case List(Damaged, _*) | List(Unknown, _*) =>
        val r = evaluateDamagedArrangement(springs.tail, damagedLeft - 1, damagedGroups).map(_.map(Damaged :: _))
        println(s"evaluateDamagedArrangement result=$r")
        r

def evaluateArrangement(springs: List[Spring], damagedGroups: List[Int]): Option[List[List[Spring]]] =
  println(s"evaluateArrangement($springs, $damagedGroups)")
  springs match
    case Nil if damagedGroups.isEmpty => Some(List(List()))
    case Nil => None
    case List(Operational, _*) => evaluateArrangement(springs.tail, damagedGroups).map(_.map(Operational :: _))
    case List(Damaged, _*) if damagedGroups.isEmpty => None
    case List(Damaged, _*) => evaluateDamagedArrangement(springs, damagedGroups.head, damagedGroups.tail)
    case List(Unknown, _*) if damagedGroups.isEmpty =>
      evaluateArrangement(springs.tail, damagedGroups).map(_.map(Operational :: _))
    case List(Unknown, _*) =>
      val a = evaluateArrangement(springs.tail, damagedGroups).map(_.map(Operational :: _))
      val b = evaluateDamagedArrangement(springs, damagedGroups.head, damagedGroups.tail)
      a.flatMap(a1 => b.map(b1 => a1 ::: b1).orElse(a)).orElse(b)

val i = 3
evaluateArrangement(springs(i).springs, springs(i).damagedGroups)

springs
  .map(r => evaluateArrangement(r.springs, r.damagedGroups))
  .map(r => r.map(_.size).getOrElse(0))
  .sum

val bigSprings = springs
  .map(r => Row(r.springs ::: Unknown :: r.springs ::: Unknown :: r.springs ::: Unknown :: r.springs ::: Unknown :: r.springs, r.damagedGroups ::: r.damagedGroups ::: r.damagedGroups ::: r.damagedGroups ::: r.damagedGroups))

// bigSprings
//   .map(r => evaluateArrangement(r.springs, r.damagedGroups))
//   .map(r => r.map(_.size).getOrElse(0))
//   .sum
