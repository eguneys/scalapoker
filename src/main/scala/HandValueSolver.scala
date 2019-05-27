package poker

case class SolverDependencies(hand: Hand) {
  val sortedCards = hand.cards.sortWith(_.rank.value > _.rank.value)

  val ranks = sortedCards.groupBy(_.rank)

  val suits = sortedCards.groupBy(_.suit)

  val wildCards = Card.wildCards(sortedCards).map(a => a.sortWith(_.rank.value > _.rank.value))

  val wildRanks = wildCards.map(_.groupBy(_.rank))



  def nbCardsByRank(rank: Rank): Int = ranks.get(rank).map(_.length).getOrElse(0)
}

trait HandValueSolver {
  val dependencies: SolverDependencies
  def solve: Option[HandValue]
}

case class HighCardSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve() = {
    Some(HighCard(sortedCards.head.rank, sortedCards.take(5)))
  }
}

case class OnePairSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve = {
    val result = ranks.find { case (rank, _) =>
      nbCardsByRank(rank) == 2
    } map {
      case (highRank, twoCards) =>
        val otherHighs = sortedCards.filterNot(twoCards.contains(_))
        (highRank, (twoCards ++ otherHighs).take(5))
    }

    result.map(r => OnePair(r._1, r._2))
  }
}

case class TwoPairSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve = {
    val result = ranks.groupBy { case (rank, _) =>
      nbCardsByRank(rank)
    } map {
      case (2, twoCardMap) if twoCardMap.size == 2 =>

        twoCardMap.toList match {
          case List((rank1, cards1), (rank2, cards2)) if (rank1.value > rank2.value) => {
            val twoCards = cards1 ++ cards2

            val otherHighs = sortedCards.filterNot(twoCards.contains(_))

            Some(TwoPair(rank1, rank2, (twoCards ++ otherHighs).take(5)))
          }
          case List((rank1, cards1), (rank2, cards2)) if (rank1.value < rank2.value) => {
            val twoCards = cards2 ++ cards1

            val otherHighs = sortedCards.filterNot(twoCards.contains(_))

            Some(TwoPair(rank2, rank1, (twoCards ++ otherHighs).take(5)))
          }
          case _ => None
        }
      case _ => None
    }
    result.find(_.isDefined).flatten
  }
}

case class ThreeOfAKindSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve = {
    val result = ranks.find { case (rank, _) =>
      nbCardsByRank(rank) == 3
    } map {
      case (highRank, threeCards) =>
        val otherHighs = sortedCards.filterNot(threeCards.contains(_))
        ThreeOfAKind(highRank, (threeCards ++ otherHighs).take(5))
    }
    result
  }
}

case class StraightSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve = {
    val oCards = findHighestStraight()
    oCards map { cards =>
      Straight(cards.head.rank, cards.take(5))
    }
  }

  def findHighestStraight(): Option[List[Card]] = Rank.allSorted.foldLeft[Option[List[Card]]](None) {
    case (None, iRank) =>
      wildCards.map(cards => {
        cards.foldLeft[List[Card]](Nil) {
          case (acc @ (head :: tail), card) if (card.rank.value + 1 == head.rank.value) => {
            card :: acc
          }
          case (Nil, card) if card.rank.value == iRank.value => {
            List(card)
          }
          case (acc, _) => acc
        }
      }).find(_.length >= 5).map(_.reverse)
    case (found, _) => found
  }
}

case class FlushSolver(dependencies: SolverDependencies) extends HandValueSolver {
  import dependencies._

  def solve = {
    suits.find {
      case (_, cards) => cards.length >= 5
    } map { 
      case (_, cards) =>
        Flush(cards.head.rank, cards.take(5))
    }
  }
}

object HandValueSolver {

  def solve(hand: Hand): HandValue = {
    val dependencies = SolverDependencies(hand)
    val all: List[HandValueSolver] = List(
      FlushSolver(dependencies),
      StraightSolver(dependencies),
      ThreeOfAKindSolver(dependencies),
      TwoPairSolver(dependencies),
      OnePairSolver(dependencies),
      HighCardSolver(dependencies))

    all.foldLeft[Option[HandValue]](None) {
      case (None, solver) => solver.solve
      case (Some(value), _) => Some(value)
    } get
  }

}
