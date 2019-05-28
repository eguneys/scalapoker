package poker

case class Move(
  act: Act,
  before: Board,
  after: Board) {

  def finalizeAfter: Board = {
    after.nextRound getOrElse after
  }

}
