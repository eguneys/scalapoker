package poker

case class ActingRounds(
  preflop: List[ActingRound],
  flop: List[ActingRound],
  turn: List[ActingRound],
  river: List[ActingRound]) {

  def isPreflop = preflop == Nil
  def isFlop = preflop != Nil && flop == Nil
  def isTurn = flop != Nil && turn == Nil
  def isRiver = turn != Nil && river == Nil

  def toList: List[List[ActingRound]] = List(preflop, flop, turn, river) match {
    case pre :: flop :: turn :: river :: Nil => List(pre, flop, turn, river)
    case pre :: flop :: turn :: Nil => List(pre, flop, turn)
    case pre :: flop :: Nil => List(pre, flop)
    case pre :: Nil => List(pre)
    case _ => Nil
  }
  
}

object ActingRounds {

  val empty: ActingRounds = ActingRounds(Nil, Nil, Nil, Nil)

  def apply(pre: List[ActingRound]): ActingRounds = ActingRounds(pre, Nil, Nil, Nil)

  implicit def fromList(list: List[List[ActingRound]]) = list match {
    case pre :: flop :: turn :: river :: Nil => ActingRounds(pre, flop, turn, river)
    case pre :: flop :: turn :: Nil => ActingRounds(pre, flop, turn, Nil)
    case pre :: flop :: Nil => ActingRounds(pre, flop, Nil, Nil)
    case pre :: Nil => ActingRounds(pre, Nil, Nil, Nil)
    case _ => ActingRounds(Nil, Nil, Nil, Nil)
  }
}
