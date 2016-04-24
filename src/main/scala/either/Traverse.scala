package either

object Traverse {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil    => Right(Nil)
    case h :: t => f(h) flatMap (hh => traverse(t)(f) map (tt => hh :: tt))
  }

  def main(args: Array[String]): Unit = {
    val l1 = List(1, 2, 3)
    val l2 = Nil
    println(traverse(l1)((a: Int) => Right(a + 1)))
    println(traverse(l2)((a: Int) => Right(a + 1)))
  }

}