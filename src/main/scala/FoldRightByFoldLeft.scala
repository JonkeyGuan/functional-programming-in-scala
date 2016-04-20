

object FoldRightByFoldLeft {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = FoldLeft.foldLeft(Reverse.reverse(as), z)((b: B, a: A) => f(a, b))

  def main(args: Array[String]): Unit = {
    println(foldRight(List("1", "2", "3", "4", "5"), "")("(" + _ + _ + ")"))
  }

}