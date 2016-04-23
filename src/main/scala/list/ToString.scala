package list

object ToString {

  def toString(l: List[Double]): List[String] = FoldRight.foldRight(l, Nil: List[String])((a: Double, b: List[String]) => a.toString() :: b)

  def main(args: Array[String]): Unit = {

    println(toString(List(1.010, 2.1, 3.2, 4.3, 5.4)))

  }

}
