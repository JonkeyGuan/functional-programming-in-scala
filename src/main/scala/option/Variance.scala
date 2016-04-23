package option

object Variance {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))

  def main(args: Array[String]): Unit = {
    val l = List(1, 20, 3, 45, 5, 6, 7.0)
    println(mean(l))
    println(variance(l))
  }
}
