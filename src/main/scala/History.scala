package poker

case class History(
  actingRounds: ActingRounds) {

  val preflop = actingRounds.isPreflop
  val river = actingRounds.isRiver

  val recentActs = actingRounds.recentActs

  val playersActedRecently = recentActs.length

  def addAct(act: Act) = 
    copy(actingRounds = actingRounds.addRecentAct(act))

  def addRound = copy(actingRounds = actingRounds.addRound)

}

object History {

  val empty = History(ActingRounds.empty)

}
