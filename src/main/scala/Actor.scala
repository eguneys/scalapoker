package poker

case class Actor(board: Board) {

  def validMoves(baseAct: Act): List[Move] =
    (Act.all flatMap {
      case Check => board.check map { move(Check, _) }
      case Call => None
      case Fold => None
      case AllIn => None
      case Raise => baseAct match {
        case r@Raise => None
        case _ => None
      }
    })

  private def move(act: Act, after: Board) = {
    Move(
      act = act,
      before = board,
      after = after)
  }
}
