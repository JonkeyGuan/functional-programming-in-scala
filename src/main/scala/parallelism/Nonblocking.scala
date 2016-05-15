package parallelism

import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Callable
import java.util.concurrent.Executors

object Nonblocking {

  sealed trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown }
      //      val future = p(es)
      //      future.apply { a => ref.set(a); latch.countDown }
      latch.await
      ref.get
    }

    def unit[A](a: A): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

    def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }

    def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] { def call = r })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None    => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None    => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil    => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

  }

  def main(args: Array[String]): Unit = {
    import Par._
    val es = Executors.newFixedThreadPool(5);
    val p = parMap(List.range(1, 10))(math.sqrt(_))
    val x = run(es)(p)
    println(x)
    es.shutdown()

    val fu = new Future[String] {
      def apply(a: String => Unit): Unit = a("123")
    }
    fu(a => println(a.toInt + 100))
  }

}