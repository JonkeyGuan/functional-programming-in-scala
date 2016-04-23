package tree

object Maximum {

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n)      => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Branch(Leaf(3), Branch(Leaf(5), Leaf(6))), Leaf(4)), Leaf(2)))
    println(maximum(tree))
  }

}
