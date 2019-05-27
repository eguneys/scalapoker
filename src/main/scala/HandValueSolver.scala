package poker

case class SolverDependencies(hand: Hand) {
  val wildCards = Card.wildSort(hand.cards)

  val highCard = wildCards.high.head.rank

  val wildRanks = wildCards.map(_.groupBy(_.rank))

  val wildSuits = wildCards.map(_.groupBy(_.rank))

  def nbCardsByRank(side: WildSide, rank: Rank): Int = wildRanks(side).get(rank).map(_.length).getOrElse(0)
}

trait HandValueSolver {
  val dependencies: SolverDependencies
  def solve: Option[HandValue]
}

case class HighCardSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve() = {
    Some(HighCard(highCard, wildCards.high))
  }
}

case class OnePairSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve = {
    val result = wildRanks.map2((wildSide, ranks) => {
      ranks.find { case (rank, card) =>
        nbCardsByRank(wildSide, rank) == 2
      } map {
        case (highRank, twoCards) =>
          val cards = wildCards(wildSide)
          val otherHighs = cards.filterNot(twoCards.contains(_))
          (highRank, twoCards ++ otherHighs)
      }
    })(High)

    result.map(r => OnePair(r._1, r._2))
  }
}

case class TwoPairSolver(dependencies: SolverDependencies) extends HandValueSolver {
  def solve = None
}

object HandValueSolver {

  def solve(hand: Hand): HandValue = {
    val dependencies = SolverDependencies(hand)
    val all: List[HandValueSolver] = List(TwoPairSolver(dependencies), OnePairSolver(dependencies), HighCardSolver(dependencies))

    all.foldLeft[Option[HandValue]](None) {
      case (None, solver) => solver.solve
      case (Some(value), _) => Some(value)
    } get
  }

}
