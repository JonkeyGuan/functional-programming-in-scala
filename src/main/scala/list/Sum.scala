package list

object Sum {

  def sum(as: List[Int]): Int = FoldLeft.foldLeft(as, 0)(_ + _)

  def main(args: Array[String]): Unit = {
    println(sum(Nil))
    println(sum(List(2)))
    println(sum(List(1, 2, 3, 4, 5)))
  }
}
