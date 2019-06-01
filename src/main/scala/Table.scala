package poker

import scalaz.Validation.FlatMap._
import scalaz.Validation.{ failureNel, success }

case class Table(stacks: Vector[Int], blinds: Int, game: Option[Game] = None) {

  val nbPlayers = stacks.count(b=>b>0)

  val capacity = stacks.length

  val minEntryStack = blinds * 10

  lazy val stacksCompact = stacks.filter(_>0).toList

  def isEmpty(index: SeatIndex): Boolean =
    stacks(index) <= 0

  def canJoinIndex(index: SeatIndex): Boolean = 
    index >= 0 && index < capacity && isEmpty(index)

  def canLeaveIndex(index: SeatIndex): Boolean = 
    index >= 0 && index < capacity && !isEmpty(index)

  def stack(seat: Int): Int = {
    val index = seat - 1
    if (index >= 0 && index < capacity)
      stacks(index)
    else
      0
  }

  def joinStack(seat: Int, stack: Int): Valid[Table] = {
    val index = seat - 1

    if (!canJoinIndex(index) || stack < minEntryStack)
      failureNel(s"cant join seat")
    else
    {
      val table = copy(
        stacks = stacks.updated(index, stack),
      )

      success(table.dealerFinalizeTable)
    }
  }

  def leaveStack(seat: Int): Option[(Table, Int)] = {
    val index = seat - 1

    if (!canLeaveIndex(index))
      None
    else {
      val stack = stacks(index)

      val table = copy(
        stacks = stacks.updated(index, 0),
      )
      Some((table.dealerFinalizeTable, stack))
    }
  }


  def deal: Valid[Table] = {
    val table = for {
      g <- game toValid "No game is playing"
      g2 <- g.deal(blinds) toValid "Cannot post blinds"
    } yield {
      val t = copy(game = Some(g2))
      t.dealerFinalizeTable
    }

    table
  }

  def playAct(act: Act): Option[(Table, Move)] =
    game flatMap { _(act) map {
      case (vg, move) =>
        val table = copy(game = Some(vg))
        (table, move)
    }
    }

  def dealerFinalizeTable: Table = game match {
    case None =>
      if (nbPlayers >= 2) {
        val newGame = Game(
          stacksCompact
        )
        copy(game = Some(newGame))
      } else {
        this
      }
    case Some(game) => {
      val table = copy(
        stacks = updateStacks(game.board.stacks)
      )
      table
    }
  }


  private def updateStacks(gameStacks: List[Int]): Vector[Int] = {
    stacks.zipWithIndex.foldLeft((stacks, gameStacks)) {
      case (acc, (0, i)) => acc
      case ((acc, gameStacks), (stack, i)) =>
        (acc.updated(i, gameStacks.head), gameStacks.tail)
    }._1
  }

  def seq(actions: Table => Valid[Table]*): Valid[Table] =
    actions.foldLeft(success(this): Valid[Table])(_ flatMap _)

  override def toString: String = {
    "Table(" +
    nbPlayers + " " +
    game + ")"
  }

}

object Table {

  def apply(capacity: Int, blinds: Int): Table =
    Table(Vector.fill(capacity)(0), blinds)

}
