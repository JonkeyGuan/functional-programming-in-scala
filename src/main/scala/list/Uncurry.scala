package list

object Uncurry {
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  
  def main(args: Array[String]): Unit = {
    def add(a: Int, b: Int): Int = a + b
    def curf = Curry.curry(add)
    def uncurryf = uncurry(curf)
    println(uncurryf(1, 2))
  }
}