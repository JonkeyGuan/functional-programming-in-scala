package option

object Sequence {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t) map (hh :: _))
  }

  def main(args: Array[String]): Unit = {
    val l = List(Some(1), Some(2), Some(3), Some(4))
    val l1 = List(Some(1), Some(2), None, Some(4))
    val l2 = Nil
    println(sequence(l))
    println(sequence(l1))
    println(sequence(l2))
  }

}