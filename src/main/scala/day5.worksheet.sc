import java.time.Duration
import java.time.Instant
import scala.collection.immutable.NumericRange
import scala.math.{max, min}
import scala.util.control.Breaks.{breakable, break}

val start = Instant.now()

val input = os.pwd / "input" / "day5.txt"
// val input = os.pwd / "input" / "day5test.txt"

val mapPattern = raw"((?:\w+)-to-(?:\w+)) map:".r
val rangePattern = raw"(\d+)\s+(\d+)\s+(\d+)".r
val seedsPattern = raw"seeds:\s+((?:\d+\s*)*)".r

extension (r: NumericRange[Long])
  // NumericRange.indexOf is very slow, this works with "my" ranges but is faster
  def myIndexOf(e: Long): Int =
    if e < r.start || e >= r.end then -1
    else (e - r.start).toInt

  // unlike mySplit(), both returned ranges are exclusive and returns Option instead of -1 on out of bounds
  def mySplitAt(i: Int): Option[(NumericRange[Long], NumericRange[Long])] =
    r match
      case x if i > 0 && i < x.end - x.start => Some((NumericRange(x.start, x.start + i, 1), NumericRange(x.start + i, x.end, 1)))
      case _ => None

case class RangeMapElement(destRange: NumericRange[Long], sourceRange: NumericRange[Long]):
  def validate: Either[Seq[String],Boolean] =
    var errs = List[String]()
    if destRange.step != 1 then errs = s"$this: destRange.step is not 1" :: errs
    if sourceRange.step != 1 then errs = s"$this: sourceRange.step is not 1" :: errs
    if destRange.isInclusive then errs = s"$this: destRange is not exclusive" :: errs
    if sourceRange.isInclusive then errs = s"$this: sourceRange is not exclusive" :: errs
    if destRange.length != sourceRange.length then errs = s"$this: range lengths are not equal" :: errs
    if sourceRange.end - sourceRange.start > Int.MaxValue then errs = s"$this sourceRange bounds exceeds MaxValue" :: errs
    if destRange.end - destRange.start > Int.MaxValue then errs = s"$this destRange bounds exceeds MaxValue" :: errs
    if sourceRange.end - sourceRange.start != sourceRange.length then errs = s"$this sourceRange bounds does not match length" :: errs
    if destRange.end - destRange.start != destRange.length then errs = s"$this destRange bounds does not match length" :: errs
    if errs.isEmpty then Right(true) else Left(errs)

  def splitSourceAt(s: Long): (RangeMapElement, RangeMapElement) =
    sourceRange.myIndexOf(s) match
      case i if i != -1 =>
        val (d1, d2) = destRange.mySplitAt(i).get
        val (s1, s2) = sourceRange.mySplitAt(i).get
        (RangeMapElement(d1, s1), RangeMapElement(d2, s2))

  def splitDestAt(d: Long): (RangeMapElement, RangeMapElement) =
    destRange.myIndexOf(d) match
      case i if i != -1 =>
        val (d1, d2) = destRange.mySplitAt(i).get
        val (s1, s2) = sourceRange.mySplitAt(i).get
        (RangeMapElement(d1, s1), RangeMapElement(d2, s2))

  override def toString(): String =
    s"RangeMapEntry((${destRange.start}-${destRange.end}(${destRange.end - destRange.start}) <- (${sourceRange.start}-${sourceRange.end}(${sourceRange.end - sourceRange.start}))))"

object RangeMapElement:
  def apply(dest: Long, source: Long, length: Long): RangeMapElement =
    RangeMapElement(NumericRange(dest, dest + length, 1L), NumericRange(source, source + length, 1L))

case class RangeMap(rangeMaps: Vector[RangeMapElement]):

  // Return RangeMap equivalent to m1(this(x))
  def *(that: RangeMap): RangeMap =
    println(s"* called ${Duration.between(Instant.now(), start).toString()}")
    val leftIterator = this.rangeMaps.sortWith((l1, l2) => (l1.destRange.start < l2.destRange.start)).iterator
    val rightIterator = that.rangeMaps.sortWith((r1, r2) => (r1.sourceRange.start < r2.sourceRange.start)).iterator
    var rm: List[RangeMapElement] = List()
    var left = leftIterator.nextOption()
    var right = rightIterator.nextOption()
    while left.isDefined || right.isDefined do
      (left, right) match
        // left before right, no overlap
        case (Some(l), Some(r)) if l.destRange.end <= r.sourceRange.start =>
          rm = l :: rm
          left = leftIterator.nextOption()

        // right before left, no overlap
        case (Some(l), Some(r)) if r.sourceRange.end <= l.destRange.start =>
          rm = r :: rm
          right = rightIterator.nextOption()

        // left before right with overlap
        case (Some(l), Some(r)) if l.destRange.start < r.sourceRange.start && l.destRange.end > r.sourceRange.start =>
          val (l1, l2) = l.splitDestAt(r.sourceRange.start)
          rm = l1 :: rm
          left = Some(l2)

        // right before left with overlap
        case (Some(l), Some(r)) if r.sourceRange.start < l.destRange.start && r.sourceRange.end > l.destRange.start =>
          val (r1, r2) = r.splitSourceAt(l.destRange.start)
          rm = r1 :: rm
          right = Some(r2)

        // left at right with same length
        case (Some(l), Some(r)) if l.destRange.start == r.sourceRange.start && l.destRange.end == r.sourceRange.end =>
          rm = RangeMapElement(r.destRange, l.sourceRange) :: rm
          left = leftIterator.nextOption()
          right = rightIterator.nextOption()

        // left at right with shorter left
        case (Some(l), Some(r)) if l.destRange.start == r.sourceRange.start && l.destRange.end < r.sourceRange.end =>
          val (r1, r2) = r.splitSourceAt(l.destRange.end)
          rm = RangeMapElement(r1.destRange, l.sourceRange) :: rm
          left = leftIterator.nextOption()
          right = Some(r2)

        // left at right with shorter right
        case (Some(l), Some(r)) if l.destRange.start == r.sourceRange.start && r.sourceRange.end < l.destRange.end =>
          val (l1, l2) = l.splitDestAt(r.sourceRange.end)
          rm = RangeMapElement(r.destRange, l1.sourceRange) :: rm
          left = Some(l2)
          right = rightIterator.nextOption()

        case (Some(l), None) =>
          rm = l :: rm
          left = leftIterator.nextOption()

        case (None, Some(r)) =>
          rm = r :: rm
          right = rightIterator.nextOption()

        case x => throw new RuntimeException(s"RangeMap.* no match for $x")

    RangeMap(rm)

  def get(x: Long): Long =
    rangeMaps.find(_.sourceRange.contains(x))
      .map(rme => (rme, rme.sourceRange.myIndexOf(x)))
      .filter(_(1) != -1)
      .map(x => x(0).destRange(x(1)))
      .getOrElse(x)

  def validate: Either[Seq[String],Boolean] =
    var errs = rangeMaps.map(_.validate.left.getOrElse(List())).flatten
    if errs.isEmpty then Right(true) else Left(errs)

object RangeMap:
  def apply(ranges: Seq[RangeMapElement]): RangeMap =
    RangeMap.apply(ranges.toVector.sortWith((r1, r2) => (r1.sourceRange.start < r2.sourceRange.start)))

val lines = os.read(input).linesIterator

val seeds = lines.next() match
  case seedsPattern(x) => x.split(raw"\s+").map(_.toLong).toList
assert(lines.next().isEmpty())

var maps = Map[String, RangeMap]()

while lines.hasNext do
  lines.next() match
    case mapPattern(m) =>
      var ranges: List[RangeMapElement] = List()
      breakable:
        while lines.hasNext do
          lines.next() match
            case rangePattern(d, s, l) => ranges = RangeMapElement(d.toLong, s.toLong, l.toLong) :: ranges
            case x if x.isEmpty() => break
      maps = maps.updated(m, RangeMap(ranges))

maps.foreach((name, rm) =>
  rm.validate match
    case Left(errs) => println(s"map $name errors:\n${errs.mkString("\n")}\n")
    case _ => ()
)

val seedToLocation = maps.get("seed-to-soil").get
  * maps.get("soil-to-fertilizer").get
  * maps.get("fertilizer-to-water").get
  * maps.get("water-to-light").get
  * maps.get("light-to-temperature").get
  * maps.get("temperature-to-humidity").get
  * maps.get("humidity-to-location").get

seedToLocation.validate match
  case Left(errs) => println(s"errors:\n${errs.mkString("\n")}\n")
  case _ => ()

// seeds.map(seedToLocation.get(_))

// println(maps.get("seed-to-soil").get)
// println(maps.get("soil-to-fertilizer").get)
// println(seedToLocation)

println(s"composition done ${Duration.between(Instant.now(), start).toString()}")

// AoC 2023 day 5 part 1 ------------------------------------------------------
val r = seeds
  .map(seedToLocation.get(_))
  .min
println(r)

println(s"number of ranges: ${seedToLocation.rangeMaps.length}")
// println(s"number of seeds: ${seeds.grouped(2).flatMap(r => r(0) until (r(0) + r(1))).length}")
println(seeds.length)

val q2 = seeds
  .grouped(2)
  .map(r => NumericRange(r(0), r(0) + r(1), 1L))
  .filter(r => r.end - r.start > Int.MaxValue)
  .toSeq

// AoC 2023 day 5 part 2 ------------------------------------------------------
val r2 = seeds
  .grouped(2)
  .flatMap(r => NumericRange(r(0), r(0) + r(1), 1L))
  .map(seedToLocation.get(_))
  .min
println(r2)

