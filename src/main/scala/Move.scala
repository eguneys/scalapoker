package poker

case class Move(
  act: Act,
  before: Board,
  after: Board) {

  def finalizeAfter: Board = {
    val nextRound = after.nextRound getOrElse after
    
    nextRound
  }

}
