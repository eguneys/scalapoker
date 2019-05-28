package poker

trait Act {
  val forsyth: Char
}

case object Check extends Act {
  val forsyth: Char = 'h'
}
case class Raise(amount: Int) extends Act {
  val forsyth: Char = 'r'
}

case object Call extends Act {
  val forsyth: Char = 'c'
}
case object Fold extends Act {
  val forsyth: Char = 'f'
}
case object AllIn extends Act {
  val forsyth: Char = 'a'
}

object Act {

  def forsyth(c: Char): Option[Act] = allByForsyth get c

  val all = List(Check, Call, Fold, AllIn)

  val allByForsyth = all map { a => a.forsyth -> a } toMap
}
