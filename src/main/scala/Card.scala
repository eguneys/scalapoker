package poker

case class Card(rank: Rank, suit: Suit) {

  def forsyth: String = rank.forsyth.toString + suit.forsyth

  override def toString = forsyth

}

object Card {

  def highSorter(a: Card, b: Card) =
    (a.rank, b.rank) match {
      case (WildRank, _) => true
      case (_, WildRank) => false
      case (NormalRank(v), NormalRank(vb)) => v > vb
      case _ => false
    }

  def lowSorter(a: Card, b: Card) =
    (a.rank, b.rank) match {
      case (WildRank, _) => false
      case (_, WildRank) => true
      case (NormalRank(v), NormalRank(vb)) => v > vb
      case _ => false
    }


  def wildSort(cards: List[Card]): WildCards[List[Card]] = WildCards(cards.sortWith(highSorter), cards.sortWith(lowSorter))

}

case class WildCards[A](high: A, low: A) {
  def apply(side: WildSide) = side match {
    case High => high
    case Low => low
  }

  def map[B](f: (A) => B): WildCards[B] =
    WildCards(f(high), f(low))

  def map2[B](f: (WildSide, A) => B): WildCards[B] =
    WildCards(f(High, high), f(Low, low))
}

trait WildSide
object High extends WildSide
object Low extends WildSide
