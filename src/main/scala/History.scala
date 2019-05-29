package poker

case class History(
  blindsPosted: Boolean,
  actingRounds: ActingRounds) {

  val preflop = actingRounds.isPreflop

  def addRound(round: List[ActingRound]) = copy(
    actingRounds = round :: Nil
  )

}

object History {

  val empty = History(false, ActingRounds.empty)

}
