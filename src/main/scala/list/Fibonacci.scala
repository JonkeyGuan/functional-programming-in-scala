package list

object Fibonacci {
  
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(i: Int, acc1: Int, acc2: Int): Int = {
      if (i == n) acc1 + acc2
      else loop(i + 1, acc2, acc1 + acc2)
    }
    if (n < 2) n
    else loop(2, 0, 1)
  }
  
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def loop(i: Int, acc1: Int, acc2: Int): Int = {
      if (i == 0) acc1
      else loop(i - 1, acc2, acc1 + acc2)
    }
    loop(n, 0, 1)
  }
  
  def main(args: Array[String]): Unit = {
    println(fib(40))
    println(fibonacci(40))
  }

}