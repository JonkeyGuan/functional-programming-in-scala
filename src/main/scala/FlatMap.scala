

object FlatMap {

  import FoldRight._
  import AppendByFoldLeft._

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a: A, b: List[B]) => append(f(a), b))

  def main(args: Array[String]): Unit = {
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
  }

}