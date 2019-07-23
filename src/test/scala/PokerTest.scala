package poker

import poker.format.{ HandVisual, PotVisual, Visual }
import org.specs2.matcher.{ Matcher, ValidationMatchers }
import org.specs2.mutable.Specification

trait PokerTest extends Specification with ValidationMatchers {

  implicit def stringToHand(str: String): Hand = HandVisual << str

  implicit def stringToList(str: String): List[Card] = HandVisual.list(str)

  implicit def stringToBoard(str: String): Board = Visual << str

  implicit def stringToPotDealer(str: String): PotDealer = PotVisual << str

  def makeBoard(blinds: Int, stacks: List[Int]): Board =
    Board.empty(blinds,
      AtLeastTwo(stacks.head,
        stacks.drop(1).head,
        stacks.drop(2).to[Vector]))

  def makeTable(blinds: Int, capacity: Int = 9): Table = {
    Table(capacity, blinds)
  }

  def makeGame(blinds: Int, stacks: List[Int]): Game = {
    Game(makeBoard(blinds, stacks))
  }

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

  def bePoss(acts: Act*): Matcher[Option[Game]] = beSome.like { case g =>
    g.moves map(_.act) must contain(exactly(acts:_*))
  }

  def bePossRaise(raise: Raise): Matcher[Option[Game]] = beSome.like { case g =>
    g.raiseMove(raise) must beSome
  }

  def bePot(visual: String): Matcher[Option[PotDealer]] = beSome.like {
    case p => p.visual must_== (PotVisual << visual).visual
  }

  def beGame(visual: String): Matcher[Option[Game]] = beSome.like {
    case p => p.board.visual must_== (Visual << visual).visual
  }
}
