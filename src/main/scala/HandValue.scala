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
