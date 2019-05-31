package poker

case class Board(
  pots: PotDealer,
  history: History) {

  val preflop = history.preflop

  val blindsPosted = pots.blindsPosted

  val actingRounds = history.actingRounds

  val stacks = pots.stacks.toList

  val players = pots.players

  val button = pots.button

  val smallBlind = pots.smallBlind

  val bigBlind = pots.bigBlind

  val firstToActOnPreflop = (bigBlind + 1) % players

  val firstToActOnFlop = (button + 1) % players

  val firstToAct = if (preflop)
    firstToActOnPreflop
  else
    firstToActOnFlop

  val recentActsSettled = pots.isSettled

  val playersActedRecently = history.playersActedRecently

  private val nextToAct = (firstToAct + playersActedRecently) % players

  val toAct = if (preflop && !blindsPosted)
    None
  else if (!recentActsSettled)
      Some(nextToAct)
    else
      None

  def nextRound: Option[Board] =
    if (!recentActsSettled)
      None
    else
      Some(copy(history = history.addRound))

  def deal(blinds: Int): Option[Board] =
    for {
      p <- pots.blinds(blinds)
    } yield copy(pots = p)

  def check: Option[Board] = addAct(Check)

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Some(this): Option[Board])(_ flatMap _)

  private def addAct(act: Act) = toAct map {
    toAct => copy(history = history.addAct(act))
  }


  def visual = format.Visual >> this

  override def toString = List(
    visual
  ) mkString "\n"
}

object Board {

  def empty(stacks: AtLeastTwo[Int]): Board =
    Board(pots = PotDealer.empty(stacks),
      history = History.empty)
}
