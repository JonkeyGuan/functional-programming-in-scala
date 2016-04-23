package tree

object SizeByFold {

  def size[A](t: Tree[A]) = Fold.fold(t)(_ => 1)(_ + _ + 1)

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Branch(Branch(Leaf(3), Branch(Leaf(5), Leaf(6))), Leaf(4)), Leaf(2)))
    println(size(tree))
  }

}