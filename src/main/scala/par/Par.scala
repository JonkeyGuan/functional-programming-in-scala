package par

import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.Executors

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val fa = a(es)
      val fb = b(es)
      UnitFuture(f(fa.get(), fb.get()))
    }

  def map2WtihTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es =>
      new Future[C] {
        val fa = a(es)
        val fb = b(es)
        def get = f(fa.get(), fb.get())
        def get(timeout: Long, timeUnit: TimeUnit) = {
          val start = System.nanoTime
          val a = fa.get(timeout, timeUnit)
          val end = System.nanoTime
          import TimeUnit.NANOSECONDS
          val b = fb.get(timeout - timeUnit.convert((end - start), NANOSECONDS), timeUnit)
          f(a, b)
        }
        def isDone = fa.isDone && fb.isDone
        def isCancelled = fa.isCancelled && fb.isCancelled
        def cancel(evenIsRunning: Boolean) = fa.cancel(evenIsRunning) || fb.cancel(evenIsRunning)
      }
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList: Par[List[Int]], unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa: Par[A], unit())((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def main(args: Array[String]): Unit = {

    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length <= 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)

        println(Thread.currentThread.getName)

        Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
      }

    val es: ExecutorService = Executors.newWorkStealingPool(10)
    val sumIt = sum(1 to 10)
    println(run(es)(sumIt).get)

    def sumWithTimeout(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length <= 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)

        println(Thread.currentThread.getName)

        Par.map2WtihTimeout(Par.fork(sumWithTimeout(l)), Par.fork(sumWithTimeout(r)))(_ + _)
      }

    println(run(es)(sortPar(unit(List(2, 1, 3, 4, 2)))).get)
    println(run(es)(map(unit(1))(_ + 100)).get)
    println(run(es)(sequence(List(unit(1), unit(2), unit(3)))).get)
    println(run(es)(parMap(List(1, 2, 3, 4, 5))(_ + 100)).get)

    //    val sumItWtihTimeout = sumWithTimeout(1 to 10)
    //    println(run(es)(sumItWtihTimeout).get(1, TimeUnit.NANOSECONDS))
    //    es.shutdown()

  }

}


