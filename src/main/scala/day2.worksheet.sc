val input = os.pwd / "input" / "day2.txt"
// val input = os.pwd / "input" / "day2test.txt"

val gamePattern = raw"Game (\d+):\s+(.*)".r
val revealSeparator = raw";\s*".r
val cubeSeparator = raw",\s*".r
val cubePattern = raw"(\d+)\s+(red|green|blue)\s*".r

case class Reveal(red: Int, green: Int, blue: Int):
  def isValid: Boolean =
    red <= 12 && green <= 13 && blue <= 14
  def power: Int =
    red * green * blue

object Reveal:
  def apply(s: String): Reveal =
    val m = cubeSeparator.split(s)
      .collect { case cubePattern(count, color) => (color, count.toInt) }
      .groupMapReduce((color, count) => color)(_(1))((x, y) => x + y)
    Reveal(m.getOrElse("red", 0), m.getOrElse("green", 0), m.getOrElse("blue", 0))

case class Game(id: Int, reveals: Seq[Reveal]):
  def isValid: Boolean =
    reveals.forall(_.isValid)
  def minimal: Reveal =
    reveals.foldLeft(Reveal(0, 0, 0))((r1, r2) => Reveal(r1.red max r2.red, r1.green max r2.green, r1.blue max r2.blue))

object Game:
  def apply(s: String): Game =
    s match
      case gamePattern(id, rs) => Game(id.toInt, revealSeparator.split(rs).map(r => Reveal(r)).toSeq)

// AoC 2023 day 2 part 1 ------------------------------------------------------
os.read(input).linesIterator
  .map(Game(_))
  .collect { case g if g.isValid => g.id }
  .sum

// AoC 2023 day 2 part 2 ------------------------------------------------------
os.read(input).linesIterator
  .map(Game(_))
  .map(_.minimal.power)
  .sum
