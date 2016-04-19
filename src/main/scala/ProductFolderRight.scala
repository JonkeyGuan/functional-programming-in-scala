

object ProductFolderRight {

  def product(ld: List[Double]): Double = {
    def loop(ls: List[Double], acc: Double): Double =
      ls match {
        case Nil      => acc
        case 0.0 :: t => 0.0
        case h :: t   => h * loop(t, acc)
      }
    loop(ld, 1.0)
  }

  def main(args: Array[String]): Unit = {
    println(product(List(1.0, 2, 3, 4, 5)))
    println(product(List(1.0)))
    println(product(List(1, 2, 0.0, 4, 5)))
  }
  
}
