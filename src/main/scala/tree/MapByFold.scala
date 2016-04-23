package tree

object MapByFold {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = Fold.fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Branch(Leaf(3), Branch(Leaf(5), Leaf(6))), Leaf(4)), Leaf(2)))
    println(map(tree)(_ * 2))
  }

}