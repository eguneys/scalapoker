package poker

case class PotBuilder(lastFullRaise: Int, bets: Map[StackIndex, Int], involved: Set[StackIndex]) {

  lazy val amount = bets.values.reduce(_+_)

  lazy val highestBet = if (bets.values.isEmpty) 0
  else bets.values.reduce(Math.max(_, _))

  lazy val minRaise = lastFullRaise

  lazy val all = bets.keys.toSet

  lazy val involvedBets = involved map (i => i -> bets(i))

  lazy val allBets = all map (i => i -> bets(i))

  def bet(index: StackIndex): Int = bets getOrElse (index, 0)

  def howOnTop(onTop: Int) =
    highestBet + onTop

  def howMore(index: StackIndex, onTop: Int) =
    howOnTop(onTop) - bet(index)

  def isFullRaise(index: StackIndex, allInBet: Int) =
    (bet(index) + allInBet) - highestBet >= lastFullRaise

  def isSettled(activePlayers: Set[StackIndex]) = activePlayers.forall(isHighest)

  def isHighest(index: StackIndex) = bets.get(index).exists(_ == highestBet)

  def toCall(index: StackIndex) = highestBet - bets.getOrElse(index, 0)

  def pots(stacks: AtLeastTwo[Int]): List[Pot] = {
    def sidePot(bets: List[Tuple2[Int, Set[StackIndex]]], foldedBets: List[Int], acc: List[Pot]): List[Pot] = bets match {
      case bet::tail if (!bet._2.subsetOf(involved)) =>
        sidePot(tail, bet._1 :: foldedBets, acc)
      case bet::tail => {
        val involved = bet._2 ++ tail.flatMap(tailBet => tailBet._2)
        val newTail = tail.map(tailBet => (tailBet._1 - bet._1, tailBet._2))
        val folded = foldedBets.foldLeft(0)(_+_)
        val newPot = Pot(bet._1 * involved.size + folded, involved.toList.sorted)
        sidePot(newTail, Nil, newPot :: acc)
      }
      case _ => acc
    }

    sidePot(allBets
      .groupBy(_._2)
      .toSeq.sortBy(_._1)
      .map(g => (g._1, g._2.map(_._1))).toList, Nil, Nil).reverse
  }

  def blinds(small: StackIndex, big: StackIndex, amount: Int): Option[PotBuilder] = if (highestBet > 0)
    None
  else for {
    b2 <- updateBet(small, amount / 2)
    b3 <- b2.updateBet(big, amount)
    b4 = b3.copy(lastFullRaise = amount)
  } yield b4

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
      for {
        d1 <- updateBet(index, highestBet)
      } yield d1

  def raise(index: StackIndex, onTop: Int): Option[PotBuilder] = if (onTop < minRaise)
    None
  else
    for {
      b1 <- updateBet(index, howOnTop(onTop))
      b2 = b1.copy(lastFullRaise = onTop)
    } yield b2

  def fold(index: StackIndex):  Option[PotBuilder] =
    Some(copy(involved = involved - index))

  def allin(index: StackIndex, amount: Int): Option[PotBuilder] = {
    val toAmount = bet(index) + amount
    for {
      b1 <- updateBet(index, toAmount)
      raiseAmount = toAmount - highestBet
      b2 = if (raiseAmount > lastFullRaise) b1.copy(lastFullRaise = raiseAmount) else b1
    } yield b2
  }

  private def updateBet(index: StackIndex, amount: Int): Option[PotBuilder] =
    Some(copy(bets = bets + (index -> amount)))
}

case class Pot(amount: Int, involved: List[StackIndex]) {

  def distribute(values: List[Int]): List[PotDistribution] = {
    val topIndexes = values.zipWithIndex
      .filter(p => involved.contains(p._2))
      .groupBy(_._1)
      .toSeq.sortWith(_._1 > _._1)
      .head._2
      .map(_._2)

    topIndexes.map(PotDistribution(_, amount / topIndexes.length))
  }

}

object PotBuilder {

  def empty(involved: List[StackIndex]) = PotBuilder(
    lastFullRaise = 0,
    bets = Map.empty[StackIndex, Int],
    involved = involved.toSet)

}

case class PotDistribution(index: StackIndex, amount: Int)
