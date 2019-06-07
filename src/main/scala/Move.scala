package poker

case class Move(
  act: Act,
  before: Board,
  after: Board) {

  def finalizeAfter: Board = {
    val board = after.updateHistory { h =>
      h.addAct(act)
    }

    board.nextRound getOrElse board
  }



}
