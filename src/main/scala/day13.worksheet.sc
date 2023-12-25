val input = os.pwd / "input" / "day13.txt"
// val input = os.pwd / "input" / "day13test.txt"

enum Ground(val symbol: Char):
  case Ash extends Ground('.')
  case Rock extends Ground('#')

object Ground:
  def apply(symbol: Char): Ground =
    Ground.values.find(_.symbol == symbol).get

val maps = os.read(input).linesIterator
  .map(_.map(Ground(_)).toVector)
  .toVector
  .foldLeft(Vector(Vector[Vector[Ground]]()))((m, r) =>
    if r.isEmpty then
      m.appended(Vector())
    else
      m.updated(m.length - 1, m.last.appended(r))
  )

maps.foreach(m =>
  m.foreach(r => println(r.map(g => g.symbol).mkString))
  println("")
)

def isEqualRow(m: Vector[Vector[Ground]], i: Int, j: Int): Boolean =
  m(i) == m(j)

def isEqualCol(m: Vector[Vector[Ground]], i: Int, j: Int): Boolean =
  m.forall(r => r(i) == r(j))

def isRowSymmetric(m: Vector[Vector[Ground]], i: Int): Boolean =
  (i to 0 by -1).zip(i+1 until m.length)
    .forall((j, k) => isEqualRow(m, j, k))

def isColSymmetric(m: Vector[Vector[Ground]], i: Int): Boolean =
  (i to 0 by -1).zip(i+1 until m.head.length)
    .forall((j, k) => isEqualCol(m, j, k))

def smudgeRow(m: Vector[Vector[Ground]], i: Int, j: Int): Int =
  m(i).zip(m(j)).filter((r, c) => r != c).length

def smudgeCol(m: Vector[Vector[Ground]], i: Int, j: Int): Int =
  m.filter(r => r(i) != r(j)).length

def isSmudgeRowSymmetric(m: Vector[Vector[Ground]], i: Int): Boolean =
  (i to 0 by -1).zip(i+1 until m.length)
    .map((j, k) => smudgeRow(m, j, k))
    .sum == 1

def isSmudgeColSymmetric(m: Vector[Vector[Ground]], i: Int): Boolean =
  (i to 0 by -1).zip(i+1 until m.head.length)
    .map((j, k) => smudgeCol(m, j, k))
    .sum == 1

// AoC 2023 day 13 part 1 -----------------------------------------------------
maps
  .map(m =>
    val rowSymmetry = (0 until m.length - 1)
      .filter(i => isEqualRow(m, i, i+1) && isRowSymmetric(m, i))
    val colSymmetry = (0 until m.head.length - 1)
      .filter(i => isEqualCol(m, i, i+1) && isColSymmetric(m, i))
    (m, rowSymmetry, colSymmetry)
  )
  .map((m, r, c) =>
    assert(r.length <= 1)
    assert(c.length <= 1)
    r.headOption.map(x => (x + 1) * 100)
      .orElse(c.headOption.map(_ + 1))
      .getOrElse(0)
  )
  .sum

// AoC 2023 day 13 part 2 -----------------------------------------------------
maps
  .map(m =>
    val smudgeRowSymmetry = (0 until m.length - 1)
      .filter(i => isSmudgeRowSymmetric(m, i))
    val smudgeColSymmetry = (0 until m.head.length - 1)
      .filter(i => isSmudgeColSymmetric(m, i))
    (m, smudgeRowSymmetry, smudgeColSymmetry)
  )
  .map((m, r, c) =>
    assert(r.length <= 1)
    assert(c.length <= 1)
    r.headOption.map(x => (x + 1) * 100)
      .orElse(c.headOption.map(_ + 1))
      .getOrElse(0)
  )
  .sum
