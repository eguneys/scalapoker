package poker

import poker.format.{ HandVisual, Visual }
import org.specs2.matcher.{ Matcher, ValidationMatchers }
import org.specs2.mutable.Specification

trait PokerTest extends Specification with ValidationMatchers {

  implicit def stringToHand(str: String): Hand = HandVisual << str

  implicit def stringToList(str: String): List[Card] = HandVisual.list(str)

  implicit def stringToBoard(str: String): Board = Visual << str

  def makeBoard(stacks: List[Int]): Board =
    Board.empty(AtLeastTwo(stacks.head,
      stacks.drop(1).head,
      stacks.drop(2).to[Vector]))

  def makeTable(blinds: Int, capacity: Int = 9): Table = {
    Table(Vector.fill(capacity)(0),
      blinds,
      None)
  }

  def makeGame(stacks: List[Int]): Game = {
    Game(makeBoard(stacks))
  }

  implicit def richTable(table: Table) = new {

    def playActs(acts: Act*): Option[Table] = playActList(acts)

    def playActList(acts: Iterable[Act]): Option[Table] = {
      val vt = acts.foldLeft(Some(table): Option[Table]) { (vt, act) =>
        val nt = vt flatMap { t => t.playAct(act) map (_._1) }
        nt
      }
      vt
    }
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

  def bePoss(acts: Act*): Matcher[Game] = { g: Game =>
    g.moves map(_.act) must contain(exactly(acts:_*))
  }

  def bePossRaise(raise: Raise): Matcher[Game] = { g: Game =>
    g.raiseMove(raise) must beSome
  }
}
