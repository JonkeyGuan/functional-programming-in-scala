package list

object AddList {

  def add(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil)             => Nil
    case (Nil, _)             => Nil
    case (h1 :: t1, h2 :: t2) => (h1 + h2) :: add(t1, t2)
  }

  def main(args: Array[String]): Unit = {
    println(add(List(1, 2, 3), List(4, 5, 6)))
  }

}