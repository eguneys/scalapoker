package poker

case class ActingRounds(
  preflop: ActingRound,
  flop: Option[ActingRound],
  turn: Option[ActingRound],
  river: Option[ActingRound]) {

  def isPreflop = flop == None
  def isFlop = flop != None && turn == None
  def isTurn = turn != None && river == None
  def isRiver = river != None

  def recentActs = if (isFlop) flop.get
  else if (isTurn) turn.get
  else if (isRiver) river.get
  else preflop

  def addRecentAct(act: Act) = if (isFlop)
    copy(flop = Some(act :: flop.get))
  else if (isTurn)
    copy(turn = Some(act :: turn.get))
  else if (isRiver)
    copy(river = Some(act :: river.get))
  else
    copy(preflop = act :: preflop)

  def addRound = if (isFlop)
    copy(turn = Some(Nil))
  else if (isTurn)
    copy(river = Some(Nil))
  else
    copy(flop = Some(Nil))

  def toList: List[ActingRound] = List(river, turn, flop).flatten :+ preflop
}

object ActingRounds {

  val empty: ActingRounds = ActingRounds(Nil, None, None, None)

    def apply(pre: ActingRound, flop: ActingRound, turn: ActingRound, river: ActingRound): ActingRounds = ActingRounds(pre, Some(flop), Some(turn), Some(river))

  def apply(pre: ActingRound, flop: ActingRound, turn: ActingRound): ActingRounds = ActingRounds(pre, Some(flop), Some(turn), None)

  def apply(pre: ActingRound, flop: ActingRound): ActingRounds = ActingRounds(pre, Some(flop), None, None)

  def apply(pre: ActingRound): ActingRounds = ActingRounds(pre, None, None, None)


  implicit def fromList(list: List[ActingRound]): ActingRounds = list match {
    case river :: turn :: flop :: preflop :: Nil => ActingRounds(preflop, flop, turn, river)
    case turn :: flop :: preflop :: Nil => ActingRounds(preflop, flop, turn)
    case flop :: preflop :: Nil => ActingRounds(preflop, flop)
    case preflop :: Nil => ActingRounds(preflop)
    case _ => ActingRounds.empty
  }

}
