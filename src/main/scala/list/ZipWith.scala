package list

object ZipWith {

  def zipWith[T](l1: List[T], l2: List[T])(f: (T, T) => T): List[T] = (l1, l2) match {
    case (_, Nil)             => Nil
    case (Nil, _)             => Nil
    case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(t1, t2)(f)
  }

  def main(args: Array[String]): Unit = {
    println(zipWith(List(1, 2, 3), List(4, 5, 6, 7))(_ + _))
  }

}