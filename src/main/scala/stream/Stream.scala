package stream

import Stream._
sealed trait Stream[+A] {

  def toList0: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList0
  }

  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _          => acc
    }
    loop(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h()))  => cons(h(), t().takeWhile(p))
    case Cons(h, t) if (!p(h())) => t().takeWhile(p)
    case _                       => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileByFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(n: Int, n1: Int): Stream[Int] = Stream.cons(n, fibs(n1, n + n1))
}

object Run {

  def main(args: Array[String]): Unit = {

    val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)

    println(s.toList0)
    println(s.toList)

    println(s.take(3))
    println(s.take(3).toList)

    println(s.takeWhile(_ % 2 == 0))
    println(s.takeWhile(_ % 2 == 0).toList)

    println(s.forAll(_ % 1 == 0))
    println(s.forAll(_ % 2 == 0))

    println(s.takeWhileByFoldRight(_ % 2 == 0))
    println(s.takeWhileByFoldRight(_ % 2 == 0).toList)

    println(s.headOption)
    println(Stream().headOption)

    println(s.map(_ + 1))
    println(s.map(_ + 1).toList)

    println(s.filter(_ % 2 == 0))
    println(s.filter(_ % 2 == 0).toList)

    println(s.append(Stream(11, 12, 13, 14)))
    println(s.append(Stream(11, 12, 13, 14)).toList)

    println(s.flatMap(x => Stream(x + 100)))
    println(s.flatMap(x => Stream(x + 100)).toList)

    println(Stream.constant(12).take(10))
    println(Stream.constant(12).take(10).toList)

    println(Stream.from(12).take(10))
    println(Stream.from(12).take(10).toList)

    println(Stream.fibs(0, 1).take(10))
    println(Stream.fibs(0, 1).take(10).toList)
  }

}