package poker

import scala.collection.immutable.ListMap

trait HandValueSolver {

  val hand: Hand

  def solve: Option[HandValue]

  val sortedCards = hand.cards.sortWith(_.rank.value > _.rank.value)

  val ranks = ListMap(sortedCards.groupBy(_.rank).toSeq.sortWith(_._1.value > _._1.value):_*)

  val suits = sortedCards.groupBy(_.suit)

  val wildCards = Card.wildCards(sortedCards).map(a => a.sortWith(_.rank.value > _.rank.value))

  val wildRanks = wildCards.map(_.groupBy(_.rank))



  def nbCardsByRank(rank: Rank): Int = ranks.get(rank).map(_.length).getOrElse(0)
}

case class HighCardSolver(hand: Hand) extends HandValueSolver {

  def solve() = {
    Some(HighCard(sortedCards.head.rank, sortedCards.take(5)))
  }
}

case class OnePairSolver(hand: Hand) extends HandValueSolver {

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

case class TwoPairSolver(hand: Hand) extends HandValueSolver {

  def solve = {
    (ranks.find { case (rank, _) =>
      nbCardsByRank(rank) == 2
    }) flatMap {
      case ((twoRank, twos)) =>
        (ranks.find { case (rank, _) =>
          nbCardsByRank(rank) == 2 && rank != twoRank
        }) map { case ((twoRank2, twos2)) =>
            val pairs = twos ++ twos2
            val otherHighs = sortedCards.filterNot(pairs.contains(_))
            TwoPair(twoRank, twoRank2, (pairs ++ otherHighs).take(5))
        }
    }
  }
}

case class ThreeOfAKindSolver(hand: Hand) extends HandValueSolver {

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

case class StraightSolver(hand: Hand) extends HandValueSolver {

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

case class FlushSolver(hand: Hand) extends HandValueSolver {

  def solve = {
    suits.find {
      case (_, cards) => cards.length >= 5
    } map { 
      case (_, cards) =>
        Flush(cards.head.rank, cards.take(5))
    }
  }
}


case class FullHouseSolver(hand: Hand) extends HandValueSolver {

  def solve = {
    (ranks.find { case (rank, _) =>
      nbCardsByRank(rank) == 3
    }) flatMap {
      case ((threeRank, threes)) =>
        (ranks.find { case (rank, _) =>
          nbCardsByRank(rank) >= 2 && rank != threeRank
        }) map { case ((twoRank, twos)) =>
            FullHouse(threeRank, twoRank, threes ++ twos.take(2))
        }
    }
  }
}

case class FourOfAKindSolver(hand: Hand) extends HandValueSolver {

  def solve = {
    (ranks.find { case (rank, _) =>
      nbCardsByRank(rank) == 4
    }) map {
      case (rank, fours) =>
        val otherHighs = sortedCards.filterNot(fours.contains(_))
        FourOfAKind(rank, (fours ++ otherHighs).take(5))
    }
  }
}

object HandValueSolver {

  def solve(hand: Hand): HandValue = {
    val all: List[HandValueSolver] = List(
      FourOfAKindSolver(hand),
      FullHouseSolver(hand),
      FlushSolver(hand),
      StraightSolver(hand),
      ThreeOfAKindSolver(hand),
      TwoPairSolver(hand),
      OnePairSolver(hand),
      HighCardSolver(hand))

    all.foldLeft[Option[HandValue]](None) {
      case (None, solver) => solver.solve
      case (Some(value), _) => Some(value)
    } get
  }

}
