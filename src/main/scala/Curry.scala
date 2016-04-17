

object Curry {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def main(args: Array[String]): Unit = {
    def add(a: Int, b: Int): Int = a + b
    def curf = curry(add)
    def cur1 = curf(2)
    println(cur1(3))
  }
}
