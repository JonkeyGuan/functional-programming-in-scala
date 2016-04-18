

object Drop {

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else if (n >= l.length) Nil
    else drop(l.tail, n - 1)
  }

  def main(args: Array[String]): Unit = {
    println(drop(List(1, 2, 3, 4, 5), 3))
    println(drop(List(1, 2, 3, 4, 5), 5))
    println(drop(List(1, 2, 3, 4, 5), 6))
    println(drop(Nil, 6))
    println(drop(Nil, -6))
    println(drop(List(1, 2), -6))
  }
}
