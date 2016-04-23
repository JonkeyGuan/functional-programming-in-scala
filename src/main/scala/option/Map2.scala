package option

object Map2 {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (a1 => b map (b1 => f(a1, b1)))

  def main(args: Array[String]): Unit = {
    val add = (a: Int, b: Int) => a + b
    println(map2(Some(1), Some(2))(add))
    println(map2(Some(1), None)(add))
    println(map2(None, None)(add))
    println(map2(None, Some(2))(add))
  }
}