

object Length {

  def length[T](ls: List[T]): Int = {
    def loop(xs: List[T], acc: Int): Int =
      xs match {
        case Nil    => acc
        case h :: t => loop(t, acc + 1)
      }
    loop(ls, 0)
  }

  def main(args: Array[String]): Unit = {
    println(length(Nil))
    println(length(List(1, 2, 3, 4)))
    println(length(List(1, 2, "a", 4)))
    println(length(List(1)))
  }

}
