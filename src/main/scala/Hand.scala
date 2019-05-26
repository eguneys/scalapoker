package poker

case class Hand(cards: List[Card]) {
  val sorted: List[Card] = value.sorted

  def value: HandValue = HandValueSolver.solve(this)
}
