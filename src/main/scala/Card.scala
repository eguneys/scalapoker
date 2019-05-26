package poker

case class Card(rank: Rank, suit: Suit) {

  def forsyth: String = rank.forsyth.toString + suit.forsyth

}

object Card {

  def highSorter(a: Card, b: Card) =
    (a.rank, b.rank) match {
      case (WildRank, _) => false
      case (_, WildRank) => true
      case (NormalRank(v), NormalRank(vb)) => v < vb
    }

  def lowSorter(a: Card, b: Card) =
    (a.rank, b.rank) match {
      case (WildRank, _) => true
      case (_, WildRank) => false
      case (NormalRank(v), NormalRank(vb)) => v < vb
    }


  def wildSort(List[Card] cards): WildCards = WildCards(cards.sortWith(highSorter), cards.sortWith(lowSorter))

}

case class WildCards(high: List[Card], low: List[Card]) {

}
