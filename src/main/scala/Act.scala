package poker

trait Act

case object Check extends Act
case class Raise(amount: Int) extends Act

case object Call extends Act
case object Fold extends Act
case object AllIn extends Act
