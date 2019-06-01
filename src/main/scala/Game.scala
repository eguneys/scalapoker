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

  def deal(blinds: Int): Option[Game] = updateBoard(_.deal(blinds))

  def endRounds(values: List[Int]) = updateBoard(_.endRounds(values))

  private def move(act: Act) = {
    def findMove(act: Act) = act match {
      case r:Raise => raiseMove(r)
      case act => moves find(_.act == act)
    }
    for {
      m <- findMove(act)
    } yield m
  }

  private def updateBoard(f: Board => Option[Board]) =
    f(board) map { b => copy(board = b) }

}

object Game {

  def apply(stacks: List[Int]): Game =
    Game(Board.empty(stacks))

}
