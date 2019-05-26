package poker

trait HandValue {

}

case class HighCard(high: Rank) extends HandValue
case class OnePair(high: Rank) extends HandValue
case class TwoPair(high1: Rank, high2: Rank) extends HandValue
