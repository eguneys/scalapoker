package poker

trait Act {
  val forsyth: Char
}

case object Check extends Act {
  val forsyth: Char = 'c'
}

case object Call extends Act {
  val forsyth: Char = 'o'
}
case object Fold extends Act {
  val forsyth: Char = 'f'
}
case object AllIn extends Act {
  val forsyth: Char = 'a'
}
case object Raise extends Act {
  val forsyth: Char = 'r'
}

case class Raise(amount: Int) extends Act {
  val forsyth: Char = 'r'
}

object Act {

  def forsyth(c: Char): Option[Act] = allByForsyth get c

  val all = List(Check, Call, Fold, AllIn, Raise)

  val allByForsyth = all map { a => a.forsyth -> a } toMap
}
