

object Polymorphic {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (as.length <= 1) true
      else if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0);
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1, 2), (a: Int, b: Int) => a < b))
    println(isSorted(Array(2, 1), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1, 2, 3, 4, 5, 6), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1, 2, 3, 3, 5, 6), (a: Int, b: Int) => a < b))
  }
}