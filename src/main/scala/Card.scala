package poker

case class Card(rank: Rank, suit: Suit) {

  def forsyth: String = rank.forsyth.toString + suit.forsyth

  override def toString = forsyth

}

object Card {
  import Rank._

  def wildCards(cards: List[Card]): WildCards[List[Card]] = WildCards(cards, cards.map {
    case Card(Ace, v) => Card(AceLow, v)
    case a => a
  })
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
