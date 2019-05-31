package poker

case class PotDealer(
  button: StackIndex,
  blindsPosted: Boolean,
  stacks: AtLeastTwo[Int],
  runningPot: PotBuilder,
  sidePots: List[Pot]) {

  val players = stacks.length

  val headsup = players == 2

  val smallBlind = if (headsup)
    button
  else
    (button + 1) % players

  val bigBlind = (smallBlind + 1) % players

  def toCall(index: StackIndex) = runningPot.toCall(index)

  lazy val isSettled = runningPot.isSettled(stacks.toList.zipWithIndex.map(_._2))

  def blinds(blinds: Int): Option[PotDealer] = for {
    d1 <- updateStacks(smallBlind, -blinds / 2)
    d2 <- d1.updateStacks(bigBlind, -blinds)
    d3 = d2.copy(blindsPosted = true)
    d4 <- d3.updatePot(_.blinds(smallBlind, bigBlind, blinds))
  } yield d4

  def check(index: StackIndex): Option[PotDealer] =
    updatePot(_.check(index))

  def call(index: StackIndex): Option[PotDealer] = for {
    d1 <- updateStacks(index, -toCall(index))
    d2 <- d1.updatePot(_.call(index))
  } yield d2

  def raise(index: StackIndex, onTop: Int): Option[PotDealer] = if (onTop < runningPot.minRaise) None
  else {
    val more = runningPot.howMore(index, onTop)
    for {
      d1 <- updateStacks(index, -more)
      d2 <- d1.updatePot(_.raise(index, onTop))
    } yield d2
  }

  private def updatePot(f: PotBuilder => Option[PotBuilder]): Option[PotDealer] = if (!blindsPosted)
    None
  else
    f(runningPot) map { r => copy(runningPot = r) }

  private def updateStacks(index: StackIndex, amount: Int): Option[PotDealer] = {
    val newAmount = stacks(index) + amount
    if (newAmount <= 0)
      None
    else
      Some(copy(stacks = stacks.updated(index, newAmount)))
  }

  def seq(actions: PotDealer => Option[PotDealer]*): Option[PotDealer] =
    actions.foldLeft(Some(this): Option[PotDealer])(_ flatMap _)

  def visual = format.PotVisual >> this

  override def toString = visual
  
}

object PotDealer {

  def empty(stacks: AtLeastTwo[Int]) = PotDealer(0, false, stacks, PotBuilder(Map.empty[StackIndex, Int]), Nil)

}
