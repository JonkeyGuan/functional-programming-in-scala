

object FoldLeft {

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil    => z
      case h :: t => foldLeft(t, f(z, h))(f)
    }

  def main(args: Array[String]): Unit = {
    println(foldLeft(List("1", "2", "3", "4", "5"), "")("(" + _ + _ + ")"))
    println(foldLeft(List(5), 0)(_ + _))
  }
}
