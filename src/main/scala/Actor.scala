package poker

case class Actor(board: Board) {

  def validMoves: List[Move] =
    (Act.all flatMap {
      case Check => board.check map { move(Check, _) }
      case Call => None
      case Fold => None
      case AllIn => None
    })

  def validRaise(raise: Raise): Option[Move] = None

  private def move(act: Act, after: Board) = {
    Move(
      act = act,
      before = board,
      after = after)
  }
}
