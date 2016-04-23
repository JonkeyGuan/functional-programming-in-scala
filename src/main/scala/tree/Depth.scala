package tree

object Depth {

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Branch(Leaf(3), Branch(Leaf(5), Leaf(6))), Leaf(4)), Leaf(2)))
    println(depth(tree))
  }

}
