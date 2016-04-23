package tree

object DepthByFold {

  def depth[A](t: Tree[A]): Int = Fold.fold(t)(_ => 0)((a, b) => (a max b) + 1)

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Branch(Leaf(3), Branch(Leaf(5), Leaf(6))), Leaf(4)), Leaf(2)))
    println(depth(tree))
  }

}
