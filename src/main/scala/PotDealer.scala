package poker

case class PotDealer(
  button: StackIndex,
  stacks: AtLeastTwo[Int],
  pots: List[Pot]) {

  def blinds(small: StackIndex, big: StackIndex, blinds: Int): Option[PotDealer] = for {
    s1 <- updateStacks(stacks, small, -blinds / 2)
    s2 <- updateStacks(s1, big, -blinds)
  } yield copy(stacks = s2)

  private def updateStacks(stacks: AtLeastTwo[Int], index: StackIndex, amount: Int): Option[AtLeastTwo[Int]] = {
    val newAmount = stacks(index) + amount
    if (newAmount < 0)
      None
    else
      Some(stacks.updated(index, newAmount))
  }

  def visual = format.PotVisual >> this
  
}

object PotDealer {

  def empty(stacks: AtLeastTwo[Int]) = PotDealer(0, stacks, Nil)

}

case class Pot(amount: Int, involved: AtLeastTwo[StackIndex]) {

}
