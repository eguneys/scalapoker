package poker
package format

object PotVisual {

  private val StackPattern = "(\\d+)(b?)".r

  private val PotPattern = "(\\d*) \\(([\\d* ?]*)\\)".r

  def <<(source: String): PotDealer = {
    val potstacks = source split "!"
    val stacks = potstacks.head split " "
    val pots = potstacks.tail

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
      pots = pots.map { _ match {
        case PotPattern(pot, groups) => 
          Pot(amount = pot.toInt,
            involved = (groups split " " toList).map (_.toInt)
          )
      }
      }.toList
    )
  }

  def >>(dealer: PotDealer): String = {
    val stacks = dealer.stacks.toList.zipWithIndex.map {
      case (stack, idx) if idx == dealer.button =>
        stack + "b"
      case (stack, idx) =>
        stack
    } mkString " "

    val pots = dealer.pots.map { pot =>
      pot.amount + " (" + (pot.involved.toList mkString " ") + ")"
    } mkString "!"

    stacks + "!" + pots
  }

}
