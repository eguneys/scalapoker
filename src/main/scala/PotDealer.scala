package poker

case class PotDealer(
  button: StackIndex,
  blindsPosted: Boolean,
  allowRaiseUntil: Option[StackIndex],
  stacks: AtLeastTwo[Int],
  runningPot: PotBuilder) {

  val players = stacks.length

  val headsup = players == 2

  val smallBlind = if (headsup)
    button
  else
    (button + 1) % players

  val bigBlind = (smallBlind + 1) % players

  lazy val playersInPot = runningPot.involved.size

  lazy val allInPlayers = stacks.toList.zipWithIndex.filter(_._1 == 0).map(_._2)

  lazy val activeBettingPlayers = (runningPot.involved -- allInPlayers)

  def toCall(index: StackIndex) = runningPot.toCall(index)

  def toAllIn(index: StackIndex) = stacks(index) - runningPot.bet(index)

  lazy val nextButton = (button + 1) % players

  lazy val stackIndexes = stacks.toList.zipWithIndex.map(_._2)

  lazy val isSettled = blindsPosted && runningPot.isSettled(activeBettingPlayers) && !allowRaiseUntil.isDefined

  lazy val pots = runningPot.pots(stacks)

  def nextRound(index: StackIndex) = copy(allowRaiseUntil = Some((index - 1 + players) % players))

  def distribute(values: List[Int]): Option[(PotDealer, Showdown)] = {
    val distributions = pots.map(_.distribute(values))
    val updated = distributions.foldLeft(Some(this): Option[PotDealer]) {
      case (d, dist) => dist.involved.foldLeft(d) {
        case (Some(d), i) => d.updateStacks(i, dist.amount / dist.involved.length)
        case _ => None
    }
    }

    updated map(_.copy(
      runningPot = PotBuilder.empty(stackIndexes),
      button = nextButton,
      blindsPosted = false,
      allowRaiseUntil = None
    ) -> distributions)
  }

  def blinds(blinds: Int): Option[PotDealer] = for {
    d1 <- updateStacks(smallBlind, -blinds / 2)
    d2 <- d1.updateStacks(bigBlind, -blinds)
    d3 = d2.copy(blindsPosted = true)
    d4 <- d3.updatePot(_.blinds(smallBlind, bigBlind, blinds))
    d5 = d4.copy(allowRaiseUntil = Some(bigBlind))
  } yield d5

  def allin(index: StackIndex): Option[PotDealer] = for {
    d1 <- checkAllowRaise(index)
    d2 <- d1.updateAllowRaise(index, runningPot.isFullRaise(index, stacks(index)))
    d3 <- d2.updatePot(_.allin(index, stacks(index)))
    d4 <- d3.updateStacks(index, -stacks(index))
  } yield d4

  def check(index: StackIndex): Option[PotDealer] = for {
    d1 <- updateAllowRaise(index, false)
    d2 <- d1.updatePot(_.check(index))
  } yield d2
  
  def call(index: StackIndex): Option[PotDealer] = for {
    d1 <- updateAllowRaise(index, false)
    d2 <- d1.updateStacks(index, -toCall(index))
    d3 <- d2.updatePot(_.call(index))
  } yield d3

  def raise(index: StackIndex, onTop: Int): Option[PotDealer] = {
    val more = runningPot.howMore(index, onTop)
    for {
      d1 <- checkAllowRaise(index)
      d2 <- d1.updateAllowRaise(index, true)
      d3 <- d2.updatePot(_.raise(index, onTop))
      d4 <- d3.updateStacks(index, -more)
    } yield d4
  }

  def fold(index: StackIndex): Option[PotDealer] = for {
    d1 <- updateAllowRaise(index, false)
    d2 <- d1.updatePot(_.fold(index))
  } yield d2

  private def checkAllowRaise(index: StackIndex): Option[PotDealer] = allowRaiseUntil map(_ => this)

  private def updateAllowRaise(index: StackIndex, fullRaise: Boolean) = if (fullRaise)
    Some(copy(allowRaiseUntil = Some((index - 1 + players) % players)))
  else allowRaiseUntil map { i =>
    if (i == index) copy(allowRaiseUntil = None)
    else this
  } orElse Some(this)

  private def updatePot(f: PotBuilder => Option[PotBuilder]): Option[PotDealer] = if (!blindsPosted)
    None
  else
    f(runningPot) map { r => copy(runningPot = r) }

  private def updateStacks(index: StackIndex, amount: Int): Option[PotDealer] = {
    val newAmount = stacks(index) + amount
    if (newAmount < 0)
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

  def empty(stacks: AtLeastTwo[Int]) = PotDealer(
    button = 0,
    blindsPosted = false,
    allowRaiseUntil = None,
    stacks = stacks,
    runningPot = PotBuilder.empty(stacks.toList.zipWithIndex.map(_._2)))

}
