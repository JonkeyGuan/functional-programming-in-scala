package list

object Filter {

  def filter[A](as: List[A])(f: A => Boolean): List[A] = FoldRight.foldRight(as, Nil: List[A])((a: A, b: List[A]) => if (f(a)) a :: b else b)

  def main(args: Array[String]): Unit = {
    println(filter(List(0, 1, 2, 3, 4, 5, 6, 7))(_ % 2 == 0))
  }

}