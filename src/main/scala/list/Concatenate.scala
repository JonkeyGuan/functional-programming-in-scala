package list

object Concatenate {

  def concat[T](l: List[List[T]]): List[T] = FoldLeft.foldLeft(l, Nil: List[T])(AppendByFoldLeft.append)

  def main(args: Array[String]): Unit = {

    println(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

  }

} 

