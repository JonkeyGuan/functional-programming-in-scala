

object Reverse {

  def reverse[T](l: List[T]): List[T] = FoldLeft.foldLeft(l, Nil: List[T])((b, a) => a :: b)

  def main(args: Array[String]): Unit = {

    println(reverse(Nil))
    println(reverse(List(1)))
    println(reverse(List(1, 2, 3, 4)))

  }
}
