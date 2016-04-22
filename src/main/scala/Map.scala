

object Map {

  def map[A, B](as: List[A])(f: A => B): List[B] = FoldRight.foldRight(as, Nil: List[B])((a: A, b: List[B]) => f(a) :: b)

  def main(args: Array[String]): Unit = {
    val l = List(1.010000, 2.1, 3.2, 4.3, 5.4)
    println(map(l)(_.toString()))
    println(map(l)(_ + 1))
  }

}