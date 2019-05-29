package poker
package format

import scalaz.NonEmptyList
import scalaz.IList

/*

 """
 10b 10 10
 C C .
 C C C
 C C C
 """, """
 
 */
object Visual {

  private val ActPattern = "(.)(\\d*)".r

  private val StackPattern = "(\\d+)(b?)".r

  private def readAct(str: String) = str match {
    case ActPattern(act, "") =>
      Act.forsyth(act(0).toLower)
    case ActPattern(act, raise) => Some(Raise(raise.toInt))
    case _ => None
  }

  private def writeAct(act: Option[Act]) = act match {
    case Some(r@Raise(amount)) => r.forsyth.toUpper + amount
    case Some(act) => act.forsyth.toUpper.toString
    case _ => "."
  }

  private def writeActs(round: List[ActingRound]) =  round.map {
    _.map { act => writeAct(Some(act)) }.toList mkString " "
  }.toList mkString "!"

  def <<(source: String): Board = {
    val lines = source.trim.lines.toList
    val stacks = lines.head split " "
    val roundActs = lines.drop(1) match {
      case Nil => NonEmptyList(stacks map (_ => ".") toList)
      case x :: xs => {
        val res = (x split "!" toList).map(_ split " " toList)
        NonEmptyList.nel(res.head, IList.fromList(res.tail))
      }
    }
    val actingRounds = lines.drop(2) match {
      case Nil => Nil
      case xs => xs.map(_.split("!").map(_.split(" ") toList) toList)
    }

    val history = History(blindsPosted = false,
      actingRounds = actingRounds map { _.map { acts =>
        acts.map { readAct(_).get }.toList: AtLeastTwo[Act]
      }})

    Board(
      _stacks = stacks.map { _ match {
        case StackPattern(stack, _) => stack.toInt
        case _ => 0
      }
      }.toList,
      blinds = 0,
      button = stacks.zipWithIndex.find {
        case (stack, _) => stack match {
          case StackPattern(stack, "b") => true
          case _ => false
        }
      }.get._2,
      history = history,
      roundActs = roundActs.map { _.map(readAct(_)): OptionActingRound }
    )
  }

  def >>(board: Board): String = {
    val stacks = board.stacks.zipWithIndex.map {
      case (stack, idx) if idx == board.button =>
        stack + "b"
      case (stack, idx) =>
        stack
    } mkString " "

    val roundActs = board.roundActs.map {
      _.map(writeAct(_)).toList mkString " "
    }.list.toList mkString "!"

    val actingRounds = List(writeActs(board.actingRounds.preflop),
      writeActs(board.actingRounds.flop),
      writeActs(board.actingRounds.turn),
      writeActs(board.actingRounds.river)) mkString "\n"

    (stacks ++ "\n" ++
      roundActs ++ "\n" ++
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
