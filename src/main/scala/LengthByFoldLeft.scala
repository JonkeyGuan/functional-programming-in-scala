

object LengthByFoldLeft {

  def length[T](as: List[T]): Int = FoldLeft.foldLeft(as, 0)((acc, _) => acc + 1)

  def main(args: Array[String]): Unit = {
    println(length(Nil))
    println(length(List(1, 2, 3, 4)))
    println(length(List(1, 2, "a", 4)))
    println(length(List(1)))
  }

}
