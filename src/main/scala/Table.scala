package poker

import scalaz.Validation.FlatMap._
import scalaz.Validation.{ failureNel, success }

case class Table(stacks: Vector[Option[Int]], blinds: Int, game: Option[Game] = None) {

  val nbPlayers = stacks.count(b=>b.isDefined)

  val capacity = stacks.length

  val minEntryStack = blinds * 10

  lazy val stacksCompact = stacks.toList.flatten

  def isEmpty(index: SeatIndex): Boolean =
    stacks.lift(index).exists(!_.isDefined)

  def isFull(index: SeatIndex): Boolean =
    stacks.lift(index).exists(_.isDefined)

  def stack(seat: Int): Option[Int] = {
    val index = seat - 1
    stacks.lift(index).flatten
  }

  def joinStack(seat: Int, stack: Int): Valid[Table] = {
    val index = seat - 1

    if (!isEmpty(index) || stack < minEntryStack)
      failureNel(s"seat is not empty")
    else
    {
      val table = copy(
        stacks = stacks.updated(index, Some(stack)),
      )

      success(table)
    }
  }

  def leaveStack(seat: Int): Valid[(Table, Int)] = {
    val index = seat - 1

    if (!isFull(index))
      failureNel(s"seat is not full")
    else {
      val stack = stacks(index).get

      val table = copy(
        stacks = stacks.updated(index, None),
      )
      success(table, stack)
    }
  }

  def deal: Valid[Table] = nbPlayers match {
    case n if n < 2 => failureNel("not enough players")
    case _ => {
      val game = Game(blinds, stacksCompact)
      val table = copy(game = game.deal)
      success(table)
    }
  }

  def showdown: Valid[(Table, Showdown)] = for {
    g <- game toValid "No game is playing"
    endGame <- g.validIf(g.shouldShowdown, "Game is not over")
    gs <- endGame.endRounds toValid "Cannot end rounds"
  } yield copy(game = Some(gs._1)) -> gs._2

  // private def updateStacks(gameStacks: List[Int]): Vector[Int] = {
  //   stacks.zipWithIndex.foldLeft((stacks, gameStacks)) {
  //     case (acc, (None, i)) => acc
  //     case ((acc, gameStacks), (Some(stack), i)) =>
  //       (acc.updated(i, Some(gameStacks.head)), gameStacks.tail)
  //   }._1
  // }

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
    Table(Vector.fill(capacity)(None), blinds)

}
