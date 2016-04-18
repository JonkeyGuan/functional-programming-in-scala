

object SetHead {

  def setHead[T](ls: List[T], head: T): List[T] = ls match {
    case Nil    => Nil
    case h :: t => head :: t
  }

  def main(args: Array[String]): Unit = {
    println(setHead(Nil, 11))
    println(setHead(List(1), 11))
    println(setHead(List(1, 2, 3), 11))
  }
}
