package poker

case class Actor(board: Board) {

  def validMoves: List[Move] =
    (Act.all flatMap {
      case Check => board.check map { move(Check, _) }
      case Call => board.call map { move(Call, _) }
      case Fold => board.fold map { move(Fold, _) }
      case AllIn => board.allin map { move(AllIn, _) }
    })

  def validRaise(raise: Raise): Option[Move] =
    board.raise(raise) map { move(raise, _) }

  private def move(act: Act, after: Board) = {
    Move(
      act = act,
      before = board,
      after = after)
  }
}
