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

  def check: Option[Board] = toAct flatMap { toAct =>
    for {
      p <- pots.check(toAct)
      h = history.addAct(Check)
      b1 = copy(pots = p, history = h)
    } yield b1
  }

  def call: Option[Board] = toAct flatMap { toAct =>
    for {
      p <- pots.call(toAct)
      h = history.addAct(Call)
      b1 = copy(pots = p, history = h)
    } yield b1
  }

  def raise(raise: Raise): Option[Board] = toAct flatMap { toAct =>
    for {
      p <- pots.raise(toAct, raise.onTop)
      h = history.addAct(raise)
      b1 = copy(pots = p, history = h)
    } yield b1
  }

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
