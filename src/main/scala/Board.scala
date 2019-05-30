package poker

import scalaz.NonEmptyList

case class Board(
  pots: PotDealer,
  roundActs: NonEmptyList[OptionActingRound],
  history: History) {

  import Board._

  val stacks = pots.stacks.toList

  val players = stacks.length

  val headsup = players == 2

  val preflop = history.preflop

  val blindsPosted = history.blindsPosted

  val actingRounds = history.actingRounds

  val button = pots.button

  val nextButton = (button + 1) % players

  val smallBlind = if (headsup)
    button
  else
    (button + 1) % players

  val bigBlind = (smallBlind + 1) % players

  val firstToActOnPreflop = (bigBlind + 1) % players

  val firstToActOnFlop = (button + 1) % players

  val firstToAct = if (preflop)
    firstToActOnPreflop
  else
    firstToActOnFlop

  val nbPlayersActed = roundActs.head
    .filter(_.isDefined).length

  val allPlayersActed = nbPlayersActed == players

  private val nextToAct = (firstToAct + nbPlayersActed) % players

  val toAct = if (preflop && !blindsPosted)
    None
  else if (!allPlayersActed)
      Some(nextToAct)
    else
      None


  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Some(this): Option[Board])(_ flatMap _)

  def nextRound: Option[Board] =
    if (!allPlayersActed)
      None
    else
      Some(copy(
        history =
          history.addRound(
            roundActs.map(_.map(_.get)).list.toList
          ),
        roundActs = emptyRoundActs(stacks)
      ))

  def deal(blinds: Int): Option[Board] =
    for {
      p <- pots.blinds(smallBlind, bigBlind, blinds)
      h = history.copy(blindsPosted = true)
    } yield copy(pots = p, history = h)

  def check: Option[Board] = addAct(Check)

  private def addAct(act: Act) = toAct map {
    toAct => 

    copy(roundActs = NonEmptyList.nel(roundActs.head.updated(toAct, Some(act)), roundActs.tail))
  }


  def visual = format.Visual >> this

  override def toString = List(
    visual
  ) mkString "\n"
}

object Board {

  def empty(stacks: AtLeastTwo[Int]): Board =
    Board(pots = PotDealer.empty(stacks),
      roundActs = emptyRoundActs(stacks),
      history = History.empty)

  def emptyRoundActs(stacks: AtLeastTwo[_]): NonEmptyList[OptionActingRound] = NonEmptyList(stacks.map(_ => None))
}
