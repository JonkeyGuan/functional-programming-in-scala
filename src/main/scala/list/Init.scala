package list

object Init {

  def init[A](l: List[A]): List[A] = {
    def loop[A](l1: List[A], i: Int): List[A] = {
      if (l1.length == 0 || l1.length <= i) Nil
      else if (l1.length == 1) l1
      else loop(l1.tail, i - 1)
    }
    loop(l, l.length - 1)
  }

  def main(args: Array[String]): Unit = {
    println(init(Nil))
    println(init(List(1)))
    println(init(List(1, 2)))
    println(init(List(1, 2, 3)))
  }
}
