package tree

object MaximumByFold {

  def maximum(t: Tree[Int]): Int = Fold.fold(t)(a => a)(_ max _)

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Branch(Leaf(3), Branch(Leaf(5), Leaf(6))), Leaf(4)), Leaf(2)))
    println(maximum(tree))
  }

}