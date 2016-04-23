package list

object AppendByFoldRight {

  def append[T](l: List[T], z: List[T]): List[T] = FoldRight.foldRight(l, z)(_ :: _)

  def main(args: Array[String]): Unit = {

    println(append(List("1", "2", "3"), List("4", "5", "6")))

  }

}
