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

  lazy val playersInPot = pots.playersInPot

  lazy val activeBettingPlayersInPot = pots.activeBettingPlayers

  lazy val playersActedRecently = history.playersActedRecently

  lazy val recentActsSettled = pots.isSettled && playersActedRecently >= playersInPot

  lazy val roundsEnd = ((river && recentActsSettled) || activeBettingPlayersInPot == 1)

  // 0 1 2 3
  // c c f r 
  // c r . c
  // c

  private lazy val foldsAndNextToAct = {
    def nextIndex(skipIndexes: List[StackIndex], i: StackIndex): StackIndex = {
      val next = (i + 1) % players
      if (skipIndexes.exists(_==next))
        nextIndex(skipIndexes, next)
      else
        next
    }

    def findFoldsAndToAct(acts: List[Act], skipIndexes: List[StackIndex], cur: StackIndex): (StackIndex, List[StackIndex]) = acts match {
      case (Fold|AllIn)::as =>
        findFoldsAndToAct(as, cur :: skipIndexes, nextIndex(skipIndexes, cur))
      case _::as =>
        findFoldsAndToAct(as, skipIndexes, nextIndex(skipIndexes, cur))
      case Nil =>
        (cur, skipIndexes)
    }

    findFoldsAndToAct(recentActs.reverse, Nil, firstToAct)
  }


  private lazy val nextToAct = foldsAndNextToAct._1

  lazy val toAct = if (preflop && !blindsPosted)
    None
  else if (!recentActsSettled && !roundsEnd)
      Some(nextToAct)
    else
      None

  def nextRound: Option[Board] = {
    if (roundsEnd || !recentActsSettled)
      None
    else
      Some(copy(history = history.addRound))
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
