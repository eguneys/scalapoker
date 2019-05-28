package poker

case class Board(
  dealer: Dealer,
  button: Int,
  roundActs: List[Act]) {

  val players = dealer.nbPlayers

  val smallBlind = (button + 1) % players
  val bigBlind = (button + 2) % players

  val firstToAct = (bigBlind + 1) % players

  val firstToActOnFlop = (button + 1) % players

  private val nextToAct = (firstToAct + roundActs.length) % players

  val allPlayersMoved = roundActs.length == players

  val toAct = if (!allPlayersMoved)
    Some(nextToAct)
  else
    None


  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Some(this): Option[Board])(_ flatMap _)

  def check: Option[Board] =
    if (!canCheck) None
    else Some(copy(roundActs = Check :: roundActs))

  def canCheck = !allPlayersMoved
}

object Board {

  def apply(nbPlayers: Int, button: Int): Board =
    Board(Dealer(nbPlayers), button, Nil)

}
