package poker

case class Game(table: Table) {

  val board = table.board

  lazy val actor = board map Actor.apply

  def moves: List[Move] = actor.map (_.validMoves) getOrElse Nil

  def raiseMove(raise: Raise): Option[Move] = actor flatMap (_.validRaise(raise))

  def apply(act: Act): Option[(Game, Move)] =
    move(act) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    copy(
      table = table.copy(board = Some(move.finalizeAfter))
    )
  }

  private def move(act: Act) = {
    def findMove(act: Act) = moves find(_.act == act)
    for {
      m <- findMove(act)
    } yield m
  }
}

object Game {

}
