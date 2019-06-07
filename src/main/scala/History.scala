package poker

case class History(
  actingRounds: ActingRounds) {

  val preflop = actingRounds.isPreflop
  val flop = actingRounds.isFlop
  val turn = actingRounds.isTurn
  val river = actingRounds.isRiver

  val recentActs = actingRounds.recentActs

  val playersActedRecently = recentActs.length

  def addAct(act: Act) =
    copy(actingRounds = actingRounds.addRecentAct(act))

  def addRound = copy(actingRounds = actingRounds.addRound)

  def endRounds = copy(actingRounds = ActingRounds.empty)

}

object History {

  val empty = History(ActingRounds.empty)

}
