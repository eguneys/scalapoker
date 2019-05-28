package poker

case class Board(
  _stacks: AtLeastTwo[Int],
  button: Int,
  roundActs: AtLeastTwo[Option[Act]],
  history: List[AtLeastTwo[Act]],
  lastRaise: Option[Raise] = None) {

  val stacks = _stacks.toList

  val players = stacks.length

  val smallBlind = (button + 1) % players
  val bigBlind = (button + 2) % players

  val firstToAct = (bigBlind + 1) % players

  val firstToActOnFlop = (button + 1) % players

  private val nextToAct = (firstToAct + roundActs.filter(_.isDefined).length) % players

  val allPlayersMoved = !roundActs.exists(!_.isDefined)

  val toAct = if (!allPlayersMoved)
    Some(nextToAct)
  else
    None


  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Some(this): Option[Board])(_ flatMap _)

  def nextRound: Option[Board] =
    if (!allPlayersMoved)
      None
    else
      Some(copy(
        history = roundActs.map(_.get) :: history,
        roundActs = _stacks.map(_ => None)
      ))



  def check: Option[Board] = addAct(Check)

  private def addAct(act: Act) = toAct map {
    toAct => 
    copy(roundActs =
      roundActs.updated(toAct, Some(act)))
  }


  def visual = format.Visual >> this

  override def toString = List(
    visual
  ) mkString "\n"
}

object Board {

  def apply(stacks: AtLeastTwo[Int], button: Int): Board =
    Board(_stacks = stacks,
      button = button,
      roundActs = stacks.map(_ => None),
      history = Nil)

}
