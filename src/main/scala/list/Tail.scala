package list

object Tail {

  def tail[T](ls: List[T]): List[T] = ls match {
    case Nil      => Nil
    case h :: Nil => Nil
    case _ :: t   => t
  }

  def main(args: Array[String]): Unit = {
    println(tail(List(1, 2, 3, 4)))
    println(tail(List(1)))
    println(tail(Nil))
  }
}
