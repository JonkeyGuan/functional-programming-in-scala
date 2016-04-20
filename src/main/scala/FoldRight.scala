

object FoldRight {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil       => z
      case ::(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def main(args: Array[String]): Unit = {
    println(foldRight(List("1", "2", "3", "4", "5"), "")("(" + _ + _ + ")"))
  }

}