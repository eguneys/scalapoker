package poker

import poker.format.{ Visual }
import org.specs2.matcher.{ ValidationMatchers }
import org.specs2.mutable.Specification

trait PokerTest extends Specification with ValidationMatchers {

  implicit def stringToHand(str: String): Hand = Visual << str

  implicit def stringToList(str: String): List[Card] = Visual.list(str)


  def makeBoard(stacks: List[Int]): Board =
    Board(AtLeastTwo(stacks.head,
      stacks.drop(1).head,
      stacks.drop(2)), 0)
}
