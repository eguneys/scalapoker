package poker
package format

object PotVisual {

  private val StackPattern = "(\\d+)(b|s|B?)".r

  private val PotBuilderPattern = "(\\d+)\\(([\\d*|\\. ?]*)\\)~".r

  private val PotPattern = "(\\d*) \\(([\\d* ?]*)\\)".r

  def <<(source: String): PotDealer = {
    val potstacks = source split "!"
    val stacks = potstacks.head split " "
    val bets = potstacks.drop(1).head

    val pots = potstacks.drop(2)

    PotDealer(
      stacks = stacks.map { _ match {
        case StackPattern(stack, _) => stack.toInt
        case _ => 0
      }
      }.toList,
      button = stacks.zipWithIndex.find {
        case (stack, _) => stack match {
          case StackPattern(stack, "b") => true
          case _ => false
        }
      }.get._2,
      blindsPosted = stacks.foldLeft(false) { 
        case (true, _) => true
        case (false, s) => s match {
          case StackPattern(_, "s") |
              StackPattern(_, "B") => true
          case _ => false
        }
      },
      runningPot = bets match {
        case PotBuilderPattern(fullRaise, bets) =>
          PotBuilder(
            fullRaise.toInt,
            (bets split " " ).toList.zipWithIndex.foldLeft(Map.empty[StackIndex, Int]) { (acc, iBet) => 
              iBet._1 match {
                case "." => acc
                case v => acc + (iBet._2 -> v.toInt)
              }
            }
          )
      },
      sidePots = pots.map(readPot) toList
    )
  }

  def >>(dealer: PotDealer): String = {
    val stacks = dealer.stacks.toList.zipWithIndex.map {
      case (stack, idx) if idx == dealer.button =>
        stack + "b"
      case (stack, idx) if dealer.blindsPosted && idx == dealer.smallBlind =>
        stack + "s"
      case (stack, idx) if dealer.blindsPosted && idx == dealer.bigBlind =>
        stack + "B"
      case (stack, idx) =>
        stack
    } mkString " "

    val runningPot = dealer.runningPot.lastFullRaise + "(" + (
      dealer.stacks.toList.zipWithIndex.map {
        case (_, i) =>
          dealer.runningPot.bets.getOrElse(i, ".")
      } mkString " ") + ")~"

    val pots = dealer.sidePots.map(writePot) mkString "!"

    stacks + "!" + runningPot + "!" + pots
  }

  private def readPot(src: String): Pot = src match {
    case PotPattern(pot, groups) =>
      Pot(amount = pot.toInt,
        involved = (groups split " " toList).map (_.toInt))
  }

  private def writePot(pot: Pot): String = {
    pot.amount + " (" + (pot.involved.toList mkString " ") + ")"
  }

}
