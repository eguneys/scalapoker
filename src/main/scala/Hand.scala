package poker

case class Hand(cards: List[Card]) {
  def sorted: List[Card] = value.sorted

  def value: HandValue = HandValueSolver.solve(this)
}
