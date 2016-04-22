

object HasSubsequence {

  def startWith[T](a: List[T], b: List[T]): Boolean = (a, b) match {
    case (_, Nil)                           => true
    case (h1 :: t1, h2 :: t2) if (h1 == h2) => startWith(t1, t2)
    case _                                  => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                      => sub == Nil
    case _ if startWith(sup, sub) => true
    case (h :: t)                 => hasSubsequence(t, sub)

  }

  def main(args: Array[String]): Unit = {
    val ls = List(1, 2, 3, 4)
    println(hasSubsequence(ls, List(1, 2)))
    println(hasSubsequence(ls, List(2, 3)))
    println(hasSubsequence(ls, List(4)))
    println(hasSubsequence(ls, List(3, 2)))
    println(hasSubsequence(ls, List(1, 2, 4)))
    println(hasSubsequence(ls, List(5)))
    println(hasSubsequence(ls, Nil))
    println(hasSubsequence(Nil, Nil))
    println(hasSubsequence(Nil, List(1)))
  }

}