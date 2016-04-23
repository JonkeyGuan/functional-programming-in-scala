package list

object FoldLeftByFoldRight {

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = FoldRight.foldRight(Reverse.reverse(l), z)((a: A, b: B) => f(b, a))

  def main(args: Array[String]): Unit = {

    println(foldLeft(List("1", "2", "3", "4", "5"), "")("(" + _ + _ + ")"))
    println(foldLeft(List(5), 0)(_ + _))

  }

}