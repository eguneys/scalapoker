package poker

import scalaz.NonEmptyList

case class Board(
  _stacks: AtLeastTwo[Int],
  button: Int,
  roundActs: NonEmptyList[OptionActingRound],
  history: History) {

  import Board._

  val stacks = _stacks.toList

  val players = stacks.length

  val headsup = players == 2

  val preflop = history.preflop

  val blindsPosted = history.blindsPosted

  val actingRounds = history.actingRounds

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

  def nextTurn: Option[Board] = None

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

  def apply(stacks: AtLeastTwo[Int], button: Int): Board =
    Board(_stacks = stacks,
      button = button,
      roundActs = emptyRoundActs(stacks),
      history = History.empty)

  def emptyRoundActs(stacks: AtLeastTwo[_]): NonEmptyList[OptionActingRound] = NonEmptyList(stacks.map(_ => None))
}
