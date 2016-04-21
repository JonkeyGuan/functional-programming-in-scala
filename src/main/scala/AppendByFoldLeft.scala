

object AppendByFoldLeft {

  def append[T](l: List[T], z: List[T]): List[T] = FoldLeft.foldLeft(Reverse.reverse(l), z)((a: List[T], b: T) => b :: a)

  def main(args: Array[String]): Unit = {

    println(append(List("1", "2", "3"), List("4", "5", "6")))

  }

}
