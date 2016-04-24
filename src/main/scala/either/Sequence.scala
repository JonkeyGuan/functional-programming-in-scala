package either

object Sequence {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil    => Right(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (tt => hh :: tt))
  }

  def main(args: Array[String]): Unit = {
    val l1 = List(Right(1), Right(2), Right(3))
    val l2 = List(Right(1), Left("Error"), Right(3))
    println(sequence(l1))
    println(sequence(l2))
  }
}