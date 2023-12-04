val input = os.pwd / "input" / "day1.txt"

// AoC 2023 day 1 part 1 ------------------------------------------------------
os.read(input).linesIterator
  .map(l => (l.find(_.isDigit).get.toString + l.reverse.find(_.isDigit).get.toString).toInt)
  .sum

val digits = Seq(
  ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"),
  ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")
)

extension (s: String)
  def replaceFirst(): String =
    digits.collect { case x if s.indexOf(x(0)) >= 0 => x :* s.indexOf(x(0)) }
      .minByOption(_(2))
      .map(x => s.patch(x(2), x(1), x(0).length()))
      .getOrElse(s)

  def replaceLast(): String =
    digits.collect { case x if s.lastIndexOf(x(0)) >= 0 => x :* s.lastIndexOf(x(0)) }
      .maxByOption(_(2))
      .map(x => s.patch(x(2), x(1), x(0).length()))
      .getOrElse(s)

// AoC 2023 day 1 part 2 ------------------------------------------------------
os.read(input).linesIterator
  .map(l => (l.replaceFirst().find(_.isDigit).get.toString + l.replaceLast().reverse.find(_.isDigit).get.toString).toInt)
  .sum
