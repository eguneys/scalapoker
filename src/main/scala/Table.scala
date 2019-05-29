package poker

case class Table(seats: Vector[Boolean], blinds: Int, board: Option[Board] = None) {

  val nbPlayers = seats.count(b=>b)

  val minEntryStack = blinds * 10

  def canJoinIndex(index: Int): Boolean = 
    index >= 0 && index < seats.length && !seats(index)

  def joinStack(seat: Int, stack: Int): Option[Table] = {
    val index = seat - 1

    if (!canJoinIndex(index) || stack < minEntryStack)
      None
    else
    {
      val table = copy(
        seats = seats.updated(index, true)
      )

      Some(table)
    }
  }


  def seq(actions: Table => Option[Table]*): Option[Table] =
    actions.foldLeft(Some(this): Option[Table])(_ flatMap _)

  override def toString: String = {
    "Table(" +
    nbPlayers + " Board(" +
    board + "))"
  }

}

object Table {

  def apply(capacity: Int, blinds: Int): Table =
    Table(Vector.fill(capacity)(false), blinds)

}
