package either

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object run {

  def main(args: Array[String]): Unit = {
    val r = Right(3)
    val l: Either[String, Int] = Left("error")

    println(r map (_ + 1))
    println(l map (_ + 1))

    println(r flatMap ((a: Int) => Right(a + 1)))
    println(l flatMap ((a: Int) => Right(a + 1)))

    println(r orElse Right(0))
    println(l orElse Right(0))

    def add(a: Int, b: Int): Int = a + b
    println(r.map2(r)(_ + _))
    println(r.map2(l)(_ + _))
    println(l.map2(r)(_ + _))
    println(l.map2(l)(_ + _))
  }

}
  