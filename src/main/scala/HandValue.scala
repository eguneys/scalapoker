package poker

trait HandValue {
  val rank: Int
  val sorted: List[Card]
}

case class HighCard(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 1
}
case class OnePair(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 2
}
case class TwoPair(high1: Rank, high2: Rank, sorted: List[Card]) extends HandValue {
  val rank = 3
}

trait HandValueSolver {
  def solve(hand: Hand): Option[HandValue]
}

object HighCard extends HandValueSolver {
  def solve(hand: Hand) = Some(HighCard(Rank.Three, Nil))
}

object OnePair extends HandValueSolver {
  def solve(hand: Hand) = None
}

object TwoPair extends HandValueSolver {
  def solve(hand: Hand) = None
}

object HandValueSolver {

  val all: List[HandValueSolver] = List(TwoPair, OnePair, HighCard)

  def solve(hand: Hand): HandValue = all.foldLeft[Option[HandValue]](None) {
    case (None, solver) => solver.solve(hand)
    case (Some(value), _) => Some(value)
  } get

}
