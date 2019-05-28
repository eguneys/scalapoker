package poker

import poker.format.{ HandVisual, Visual }
import org.specs2.matcher.{ ValidationMatchers }
import org.specs2.mutable.Specification

trait PokerTest extends Specification with ValidationMatchers {

  implicit def stringToHand(str: String): Hand = HandVisual << str

  implicit def stringToList(str: String): List[Card] = HandVisual.list(str)

  implicit def stringToBoard(str: String): Board = Visual << str

  def makeBoard(stacks: List[Int]): Board =
    Board(AtLeastTwo(stacks.head,
      stacks.drop(1).head,
      stacks.drop(2)), 0)

  def makeGame(stacks: List[Int]): Game =
    Game(makeBoard(stacks))

  implicit def richGame(game: Game) = new {

    def playActs(acts: Act*): Option[Game] = playActList(acts)

    def playActList(acts: Iterable[Act]): Option[Game] = {
      val vg = acts.foldLeft(Some(game): Option[Game]) { (vg, act) =>
        val ng = vg flatMap { g => g(act) map (_._1) }
        ng
      }
      vg
    }
  }
}
