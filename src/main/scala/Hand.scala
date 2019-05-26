package poker

case class Hand(cards: List[Card]) {
  val sorted: List[Card] = Nil

  def value: HandValue = ???
}
