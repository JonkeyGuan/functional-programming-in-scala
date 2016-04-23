package option

object Traverse {

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil    => Some(Nil)
    case h :: t => Map2.map2(f(h), traverse(t)(f))(_ :: _)
  }

  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
    val l1 = Nil

    def add1(x: Int): Option[Int] = Some(x + 1)

    println(traverse(l)(add1))
    println(traverse(l1)(add1))
  }
}
