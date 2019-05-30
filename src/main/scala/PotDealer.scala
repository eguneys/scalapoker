package poker

case class PotDealer(
  button: StackIndex,
  stacks: AtLeastTwo[Int],
  runningPot: PotBuilder,
  sidePots: List[Pot]) {

  def blinds(small: StackIndex, big: StackIndex, blinds: Int): Option[PotDealer] = for {
    d1 <- updateStacks(small, -blinds / 2)
    d2 <- d1.updateStacks(big, -blinds)
    p = d2.runningPot.bet(small, blinds / 2)
    p2 = p.bet(big, blinds)
  } yield d2.copy(runningPot = p2)

  private def updateStacks(index: StackIndex, amount: Int): Option[PotDealer] = {
    val newAmount = stacks(index) + amount
    if (newAmount < 0)
      None
    else
      Some(copy(stacks = stacks.updated(index, newAmount)))
  }

  def visual = format.PotVisual >> this

  override def toString = visual
  
}

object PotDealer {

  def empty(stacks: AtLeastTwo[Int]) = PotDealer(0, stacks, PotBuilder(Map.empty[StackIndex, Int]), Nil)

}

case class PotBuilder(bets: Map[StackIndex, Int]) {

  lazy val amount = bets.values.reduce(_+_)

  def bet(index: StackIndex, amount: Int): PotBuilder = {
    copy(bets = bets + (index -> amount))
  }

}

case class Pot(amount: Int, involved: AtLeastTwo[StackIndex]) {


}
