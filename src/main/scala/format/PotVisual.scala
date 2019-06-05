package poker
package format

object PotVisual {

  private val StackPattern = "(\\d+)(b|s|B?)".r

  private val PotBuilderPattern = "(\\d+)\\(([\\d*|\\. ?]*)\\)~".r

  private val BettingActionPattern = "(\\d+?)".r

  def <<(source: String): PotDealer = {
    val potstacks = source split "!"
    val stacks = potstacks.head split " "
    val bets = potstacks.drop(1).head

    val bettingActions = potstacks.drop(2).headOption

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
      allowRaiseUntil = bettingActions flatMap {
        case BettingActionPattern(index) => Some(index.toInt)
        case BettingActionPattern("") => None
      },
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
            lastFullRaise = fullRaise.toInt,
            bets = (bets split " " ).toList.zipWithIndex.foldLeft(Map.empty[StackIndex, Int]) { (acc, iBet) => 
              iBet._1 match {
                case "." => acc
                case v => acc + (iBet._2 -> v.toInt)
              }
            },
            involved = stacks.toList.zipWithIndex.map(_._2).toSet
          )
      }
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

    val bettingAction = dealer.allowRaiseUntil getOrElse ""

    stacks + "!" + runningPot + "!" + bettingAction
  }
}
