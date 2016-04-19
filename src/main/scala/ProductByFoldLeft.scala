

object ProductByFoldLeft {

  def product(as: List[Double]): Double = FoldLeft.foldLeft(as, 1.0)(_ * _)

  def main(args: Array[String]): Unit = {
    println(product(List(1.0, 2, 3, 4, 5)))
    println(product(List(1.0)))
    println(product(List(1, 2, 0.0, 4, 5)))
  }

}
