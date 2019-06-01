package poker

case class PotBuilder(bets: Map[StackIndex, Int]) {

  lazy val players = bets.keys

  lazy val amount = bets.values.reduce(_+_)

  lazy val highestBet = if (bets.values.isEmpty) 0
  else bets.values.reduce(Math.max(_, _))

  lazy val lowestBet = if(bets.values.isEmpty) 0
  else bets.values.reduce(Math.min(_, _))

  lazy val minRaise = highestBet - lowestBet

  def howOnTop(onTop: Int) =
    highestBet + onTop

  def howMore(index: StackIndex, onTop: Int) =
    howOnTop(onTop) - (bets getOrElse (index, 0))

  def isSettled = bets.keys.forall(isHighest)

  def isHighest(index: StackIndex) = bets.get(index).exists(_ == highestBet)

  def toCall(index: StackIndex) = highestBet - bets.getOrElse(index, 0)

  def addAllToPot(indexes: List[StackIndex]) = 
    indexes.foldLeft(Some(this): Option[PotBuilder]) { (acc, index) =>
      acc flatMap (_.updateBet(index, 0))
    }

  def blinds(small: StackIndex, big: StackIndex, indexes: List[StackIndex], amount: Int): Option[PotBuilder] = if (highestBet > 0)
    None
  else for {
    b1 <- addAllToPot(indexes)
    b2 <- b1.updateBet(small, amount / 2)
    b3 <- b2.updateBet(big, amount)
  } yield b3

  def bet(index: StackIndex, amount: Int): Option[PotBuilder] = None

  def check(index: StackIndex): Option[PotBuilder] = 
    if (isHighest(index))
      Some(this)
    else {
      None
    }

  def call(index: StackIndex): Option[PotBuilder] = 
    if (isHighest(index))
      None
    else
      updateBet(index, highestBet)

  def raise(index: StackIndex, onTop: Int): Option[PotBuilder] =
    updateBet(index, howOnTop(onTop))

  def fold(index: StackIndex):  Option[PotBuilder] =
    Some(copy(bets = bets - index))

  private def updateBet(index: StackIndex, amount: Int): Option[PotBuilder] =
    Some(copy(bets = bets + (index -> amount)))
}

case class Pot(amount: Int, involved: AtLeastTwo[StackIndex]) {

}
