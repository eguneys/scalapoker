package poker

case class Game(board: Board) {

  lazy val actor = Actor(board)

  lazy val moves: List[Move] = actor.validMoves

  lazy val dealer: Dealer = Dealer()

  def raiseMove(raise: Raise): Option[Move] = actor.validRaise(raise)

  def shouldDeal: Boolean = !board.blindsPosted

  def shouldShowdown: Boolean = board.roundsEnd

  def toAct: Option[StackIndex] = board.toAct

  def flop: List[Card] = dealer.flop

  def turn: Card = dealer.turn

  def river: Card = dealer.river

  def hands = dealer.hands(board.players)

  def handValues = hands.map(_.value.magic)

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

  def endRounds: Option[(Game, Showdown)] = {
    board.endRounds(handValues) map {
      case (board, showdown) =>
        copy(board = board) -> showdown
    }
  }

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
