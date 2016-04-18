

object DropWhile {

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil    => Nil
    case h :: t => if (f(h)) dropWhile(t, f) else h :: t
  }

  def main(args: Array[String]): Unit = {
    println(dropWhile(Nil, (a: Int) => a < 0))
    println(dropWhile(List(-1, -3, -2, 3, 4, 5, 6), (a: Int) => a < 0))
  }
}