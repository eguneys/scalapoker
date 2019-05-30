package poker

case class Game(board: Board) {

  lazy val actor = Actor(board)

  def moves: List[Move] = actor.validMoves

  def raiseMove(raise: Raise): Option[Move] = actor.validRaise(raise)

  def apply(act: Act): Option[(Game, Move)] =
    move(act) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    copy(
      board = move.finalizeAfter
    )
  }

  def deal(blinds: Int): Option[Game] = board.deal(blinds) map { b => copy(board = b) }

  private def move(act: Act) = {
    def findMove(act: Act) = moves find(_.act == act)
    for {
      m <- findMove(act)
    } yield m
  }
}

object Game {

  def apply(stacks: List[Int]): Game =
    Game(Board(stacks, button = 0))

}
