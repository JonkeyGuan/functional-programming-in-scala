package list

object Add1 {

  def add1(l: List[Int]): List[Int] = FoldRight.foldRight(l, Nil: List[Int])((a: Int, b: List[Int]) => a + 1 :: b)

  def main(args: Array[String]): Unit = {

    println(add1(List(1, 2, 3, 4, 5, 6, 7)))

  }

}