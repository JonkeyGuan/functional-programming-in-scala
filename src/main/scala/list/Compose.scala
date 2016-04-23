package list

object Compose {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    def int2String(a: Int): String = a.toString
    def stringLength(b: String): Int = b.length
    def composed = compose(stringLength, int2String)
    println(composed(1234))
  }
}