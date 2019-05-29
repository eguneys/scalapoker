package poker

final case class AtLeastTwo[A](first: A, second: A, tail: Vector[A] = Vector.empty) {

  def toList: List[A] = first :: second :: tail.toList

  def size: Int = 2 + tail.size

  def length: Int = size

  def apply(i: Int): A = i match {
    case 0 => first
    case 1 => second
    case n => tail(n - 2)
  }

  def map[B](f: A => B): AtLeastTwo[B] =
    AtLeastTwo(f(first), f(second), tail.map(f))

  def filter(p: A => Boolean): List[A] = {
    val ftail = tail.filter(p).toList
    (p(first), p(second)) match {
      case (false, true) => second :: ftail
      case (false, false) => ftail
      case (true, true) => first :: second :: ftail
      case (true, false) => first :: ftail
    }
  }

  def exists(p: A => Boolean): Boolean =
    p(first) || p(second) || tail.exists(p)

  def updated(i: Int, elem: A): AtLeastTwo[A] = 
    i match {
      case 0 => copy(first = elem)
      case 1 => copy(second = elem)
      case n => copy(tail = tail.updated(n - 2, elem))
    }
}

object AtLeastTwo {

  implicit def fromList[A](list: List[A]): AtLeastTwo[A] =
    list match {
      case first :: second :: tail =>
        AtLeastTwo(first, second, tail.to[Vector])
      case _ => throw new Exception("not enough size")
    }

}
