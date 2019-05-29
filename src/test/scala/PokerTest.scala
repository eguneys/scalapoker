package poker

import poker.format.{ HandVisual, Visual }
import org.specs2.matcher.{ Matcher, ValidationMatchers }
import org.specs2.mutable.Specification

trait PokerTest extends Specification with ValidationMatchers {

  implicit def stringToHand(str: String): Hand = HandVisual << str

  implicit def stringToList(str: String): List[Card] = HandVisual.list(str)

  implicit def stringToBoard(str: String): Board = Visual << str

  def makeBoard(stacks: List[Int], blinds: Int = 10): Board =
    Board(
      stacks = AtLeastTwo(stacks.head,
        stacks.drop(1).head,
        stacks.drop(2)),
      blinds = blinds,
      button = 0)

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

  def bePoss(acts: Act*): Matcher[Game] = { g: Game =>
    g.moves map(_.act) must contain(exactly(acts:_*))
  }

  def bePossRaise(raise: Raise): Matcher[Game] = { g: Game =>
    g.raiseMove(raise) must beSome
  }
}
