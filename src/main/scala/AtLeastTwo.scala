package poker

final case class AtLeastTwo[A](first: A, second: A, tail: List[A]) {

  def toList: List[A] = first :: second :: tail

  def size: Int = 2 + tail.size

  def length: Int = size

}
