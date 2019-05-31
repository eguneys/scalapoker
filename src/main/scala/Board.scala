package poker

case class Board(
  pots: PotDealer,
  history: History) {

  val preflop = history.preflop

  val river = history.river

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


  val playersActedRecently = history.playersActedRecently

  val recentActsSettled = pots.isSettled && playersActedRecently >= players

  private val nextToAct = (firstToAct + playersActedRecently) % players

  val toAct = if (preflop && !blindsPosted)
    None
  else if (!recentActsSettled)
      Some(nextToAct)
    else
      None

  def nextRound: Option[Board] =
    if (!recentActsSettled || river)
      None
    else
      Some(copy(history = history.addRound))

  def deal(blinds: Int): Option[Board] =
    for {
      p <- pots.blinds(blinds)
    } yield copy(pots = p)

  def check: Option[Board] = for {
    toAct <- toAct
    p <- pots.check(toAct)
  } yield copy(pots = p)

  def call: Option[Board] = for {
    toAct <- toAct
    p <- pots.call(toAct)
  } yield copy(pots = p)

  def raise(raise: Raise): Option[Board] = for {
    toAct <- toAct
    p <- pots.raise(toAct, raise.onTop)
  } yield copy(pots = p)

  def fold: Option[Board] = for {
    toAct <- toAct
    p <- pots.fold(toAct)
  } yield copy(pots = p)

  def updateHistory(f: History => History) = 
    copy(history = f(history))

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Some(this): Option[Board])(_ flatMap _)

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
