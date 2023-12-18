val input = os.pwd / "input" / "day7.txt"
// val input = os.pwd / "input" / "day7test.txt"

val handPattern = raw"([1-9TJQKA]{5})\s+(\d+)".r

enum Game:
  case CamelCards, JokerCamelCards

import Game.*

enum Card(val label: String, val games: Set[Game]) extends Ordered[Card]:
  case Joker extends Card("J", Set(JokerCamelCards))
  case Two extends Card("2", Set(CamelCards, JokerCamelCards))
  case Three extends Card("3", Set(CamelCards, JokerCamelCards))
  case Four extends Card("4", Set(CamelCards, JokerCamelCards))
  case Five extends Card("5", Set(CamelCards, JokerCamelCards))
  case Six extends Card("6", Set(CamelCards, JokerCamelCards))
  case Seven extends Card("7", Set(CamelCards, JokerCamelCards))
  case Eight extends Card("8", Set(CamelCards, JokerCamelCards))
  case Nine extends Card("9", Set(CamelCards, JokerCamelCards))
  case Ten extends Card("T", Set(CamelCards, JokerCamelCards))
  case Jack extends Card("J", Set(CamelCards))
  case Queen extends Card("Q", Set(CamelCards, JokerCamelCards))
  case King extends Card("K", Set(CamelCards, JokerCamelCards))
  case Ace extends Card("A", Set(CamelCards, JokerCamelCards))

  def compare(that: Card): Int =
    this.ordinal.compare(that.ordinal)

import  Card.*

enum Kind extends Ordered[Kind]:
  case HighCard
  case OnePair
  case TwoPair
  case ThreeOfAKind
  case FullHouse
  case FourOfAKind
  case FiveOfAKind

  def compare(that: Kind): Int =
    this.ordinal.compare(that.ordinal)

case class Hand(cards: Seq[Card], bid: Int) extends Ordered[Hand]:
  def kind(): Kind =
    var ranks = cards.groupMapReduce(c => c)(c => 1)(_ + _)
    val jokers = ranks.get(Joker).getOrElse(0)
    ranks = ranks.removed(Joker)
    (ranks.values.toIndexedSeq.sorted.reverse, jokers) match
      case (Seq(5), 0) | (Seq(4), 1) | (Seq(3), 2) | (Seq(2), 3) | (Seq(1), 4) | (Seq(), 5) => Kind.FiveOfAKind
      case (Seq(4, _*), 0) | (Seq(3, _*), 1) | (Seq(2, _*), 2) | (Seq(1, _*), 3) => Kind.FourOfAKind
      case (Seq(3, 2, _*), 0) | (Seq(2, 2, _*), 1) => Kind.FullHouse
      case (Seq(3, _*), 0) | (Seq(2, _*), 1) | (Seq(1, _*), 2) => Kind.ThreeOfAKind
      case (Seq(2, 2, _*), 0) | (Seq(2, 1, _*), 1) => Kind.TwoPair
      case (Seq(2, _*), 0) | (Seq(1, _*), 1) => Kind.OnePair
      case _ => Kind.HighCard

  def compare(that: Hand): Int =
    this.kind().compare(that.kind()) match
      case x if x == 0 =>
        this.cards.zip(that.cards).map((t, o) => t.compare(o)).find(_ != 0).get
      case x => x

object Hand:
  def camelCards(s: String): Hand =
    s match
      case handPattern(cs, b) =>
        new Hand(cs.map(ch => Card.values.find(c => ch.toString() == c.label && c.games.contains(CamelCards)).get), b.toInt)
  def jokerCamelCards(s: String): Hand =
    s match
      case handPattern(cs, b) =>
        new Hand(cs.map(ch => Card.values.find(c => ch.toString() == c.label && c.games.contains(JokerCamelCards)).get), b.toInt)

// AoC 2023 day 7 part 1 ------------------------------------------------------
val hands = os.read(input).linesIterator
  .map(Hand.camelCards(_))
  .toSeq
  .sorted.zipWithIndex.map((c, i) => c.bid * (i + 1)).sum

  // AoC 2023 day 7 part 2 ------------------------------------------------------
val hands2 = os.read(input).linesIterator
  .map(Hand.jokerCamelCards(_))
  .toSeq
  .sorted.zipWithIndex.map((c, i) => c.bid * (i + 1)).sum
