package poker
package format

/*
 """
 10b 10 10!(. . .)~!10 (0 1 2)
 H H
 H H H R R R C C C
 H H H H H H C C R H H H H H
 """
 */
object Visual {

  private val ActPattern = "(.)(\\d*)".r

  private def readAct(str: String) = str match {
    case ActPattern(act, "") =>
      Act.forsyth(act(0).toLower).get
    case ActPattern(act, raise) => Raise(raise.toInt)
  }

  private def writeAct(act: Act) = act match {
    case r@Raise(amount) => r.forsyth.toUpper.toString + amount
    case act => act.forsyth.toUpper.toString
  }

  private def writeActs(round: ActingRound) =  round.map { act => writeAct(act) } mkString " "

  def <<(source: String): Board = {
    val lines = source.trim.lines.toList
    val potsSource = lines.head
    val pots = PotVisual << potsSource
    val actingRounds = lines.drop(1).map(_.split(" ") toList)

    val history = History(actingRounds = actingRounds map { case List("") => Nil
      case l => l.map(readAct).toList })

    Board(pots = pots,
      history = history)
  }

  def >>(board: Board): String = {
    val pots = PotVisual >> board.pots

    val actingRounds = board.actingRounds.toList map(writeActs _) mkString "\n"

    (pots ++ "\n" ++
      actingRounds).trim
  }

  def addNewLines(str: String) = "\n" + str + "\n"
}

// "Kh Tc 5d As 3c 3s 2h"
object HandVisual {

  def >>(hand: Hand): String = hand.cards.map(_.forsyth) mkString " "

  def <<(visual: String): Hand = {
    Hand(list(visual))
  }

  def list(visual: String): List[Card] = {
    val l = visual split " "
    for {
      card <- l.toList
      rank <- Rank forsyth card.charAt(0)
      suit <- Suit forsyth card.charAt(1)
    } yield Card(rank, suit)
  }

}
