package tree

object Fold {

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Branch(Leaf(3), Branch(Leaf(5), Leaf(6))), Leaf(4)), Leaf(2)))
    println(fold(tree)(_ => 1)(_ + _ + 1))
  }

}