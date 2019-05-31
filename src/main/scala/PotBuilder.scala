package poker

case class PotBuilder(bets: Map[StackIndex, Int]) {

  lazy val amount = bets.values.reduce(_+_)

  lazy val highestBet = bets.values.foldLeft(0)((acc, v) => Math.max(acc, v))

  lazy val secondHighestBet = bets.values.foldLeft(0) {
    case (acc, v) if v == highestBet => acc
    case (acc, v) => Math.max(acc, v)
  }

  lazy val minRaise = highestBet - secondHighestBet

  def howOnTop(onTop: Int) =
    highestBet + onTop

  def howMore(index: StackIndex, onTop: Int) =
    howOnTop(onTop) - (bets getOrElse (index, 0))

  def isSettled(indexes: List[StackIndex]) = indexes.forall(isHighest)

  def isHighest(index: StackIndex) = bets.get(index).exists(_ == highestBet)

  def toCall(index: StackIndex) = highestBet - bets.getOrElse(index, 0)

  def blinds(small: StackIndex, big: StackIndex, amount: Int): Option[PotBuilder] = if (highestBet > 0)
    None
  else
    for {
      b1 <- updateBet(small, amount / 2)
      b2 <- b1.updateBet(big, amount)
    } yield b2

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

  private def updateBet(index: StackIndex, amount: Int): Option[PotBuilder] =
    Some(copy(bets = bets + (index -> amount)))
}

case class Pot(amount: Int, involved: AtLeastTwo[StackIndex]) {

}
