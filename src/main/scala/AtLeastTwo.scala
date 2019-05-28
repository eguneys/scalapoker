package poker

final case class AtLeastTwo[A](first: A, second: A, tail: List[A]) {

  def toList: List[A] = first :: second :: tail

  def size: Int = 2 + tail.size

  def length: Int = size

  def map[B](f: A => B): AtLeastTwo[B] =
    AtLeastTwo(f(first), f(second), tail.map(f))

  def zipWith[B, C](b: AtLeastTwo[B])(f: (A, B) => C): AtLeastTwo[C] = {

    def zwRev(as: List[A], bs: List[B], acc: List[C]): List[C] = (as, bs) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (x :: xs, y :: ys) => zwRev(xs, ys, f(x, y) :: acc)
    }

    AtLeastTwo(
      f(first, b.first),
      f(second, b.second),
      zwRev(tail, b.tail, Nil).reverse)
  }

  def filter(p: A => Boolean): List[A] = {
    val ftail = tail.filter(p)
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
        AtLeastTwo(first, second, tail)
      case _ => throw new Exception("not enough size")
    }

}
