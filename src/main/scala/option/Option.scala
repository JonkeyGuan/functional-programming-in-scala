package option

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None    => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _    => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _                 => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Run {

  def main(args: Array[String]): Unit = {
    val some = Some(1.01000)
    val none = None: Option[Double]

    println(some.map(_ + 1))
    println(none.map(_ + 1))

    println(some.flatMap(x => Some(x + 1)))
    println(none.flatMap(x => Some(x + 1)))

    println(some.getOrElse("3.14"))
    println(none.getOrElse("3.14"))

    println(some.orElse(Some(1.414)))
    println(none.orElse(Some(1.414)))

    println(some.filter(x => x == 1.01))
    println(none.filter(x => x == 1.01))

  }

}
