package poker

case class Board(
  pots: PotDealer,
  history: History) {

  val preflop = history.preflop

  val river = history.river

  val blindsPosted = pots.blindsPosted

  val actingRounds = history.actingRounds

  val recentActs = history.recentActs

  val stacks = pots.stacks.toList

  val players = pots.players

  val button = pots.button

  lazy val smallBlind = pots.smallBlind

  lazy val bigBlind = pots.bigBlind

  lazy val firstToActOnPreflop = (bigBlind + 1) % players

  lazy val firstToActOnFlop = (button + 1) % players

  lazy val firstToAct = if (preflop)
    firstToActOnPreflop
  else
    firstToActOnFlop

  lazy val activeBettingPlayersInPot = pots.activeBettingPlayers.size

  lazy val playersActedRecently = history.playersActedRecently

  lazy val recentActsSettled = pots.isSettled

  lazy val roundsEnd = ((river && recentActsSettled) || activeBettingPlayersInPot == 1)

  // 0 1 2 3
  // c c f r 
  // c r . c
  // c

  private lazy val nextToAct = {
    def nextIndex(skipIndexes: List[StackIndex], i: StackIndex): StackIndex = {
      val next = (i + 1) % players
      if (skipIndexes.exists(_==next))
        nextIndex(skipIndexes, next)
      else
        next
    }

    def findToAct(acts: List[Act], skipIndexes: List[StackIndex], cur: StackIndex): StackIndex = acts match {
      case (Fold|AllIn)::as =>
        findToAct(as, cur :: skipIndexes, nextIndex(skipIndexes, cur))
      case _::as =>
        findToAct(as, skipIndexes, nextIndex(skipIndexes, cur))
      case Nil =>
        cur
    }

    findToAct(recentActs.reverse, Nil, firstToAct)
  }

  lazy val toAct = if (preflop && !blindsPosted)
    None
  else if (!(recentActsSettled || roundsEnd)) {
    Some(nextToAct)
  }
    else
      None

  def nextRound: Option[Board] = {
    if (roundsEnd || !recentActsSettled)
      None
    else
      for {
        d1 <- Some(copy(history = history.addRound))
        d2 = d1.copy(pots = d1.pots.nextRound(d1.firstToAct))
      } yield d2
  }

  def endRounds(values: List[Int]): Option[Board] = {
    if (!roundsEnd)
      None
    else
      for {
        p <- pots.distribute(values)
        h = history.endRounds
      } yield copy(pots = p, history = h)
  }

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

  def allin: Option[Board] = for {
    toAct <- toAct
    p <- pots.allin(toAct)
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
