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

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(pa, map2(pb, pc) { (b, c) => (b, c) }) { (a, bc) =>
      {
        val (b, c) = bc
        f(a, b, c)
      }
    }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(pa, map2(pb, map2(pc, pd) { (c, d) => (c, d) }) { (b, cd) => (b, cd) }) { (a, bcd) =>
      {
        val (b, (c, d)) = bcd
        f(a, b, c, d)
      }
    }
  }
  def map5[A, B, C, D, E, F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    map2(pa, map2(pb, map2(pc, map2(pd, pe) { (d, e) => (d, e) }) { (c, de) => (c, de) }) { (b, cde) => (b, cde) }) { (a, bcde) =>
      {
        val (b, (c, (d, e))) = bcde
        f(a, b, c, d, e)
      }
    }
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def main(args: Array[String]): Unit = {

    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length <= 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)

        println(Thread.currentThread.getName)

        Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
      }

    val es: ExecutorService = Executors.newCachedThreadPool()
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

    //    try {
    //      val sumItWtihTimeout = sumWithTimeout(1 to 10)
    //      println(run(es)(sumItWtihTimeout).get(1, TimeUnit.NANOSECONDS))
    //      es.shutdown()
    //    } catch {
    //      case e: Exception => println(e.toString())
    //    }

    println(run(es)(sortPar(unit(List(2, 1, 3, 4, 2)))).get)
    println(run(es)(map(unit(1))(_ + 100)).get)
    println(run(es)(sequence(List(unit(1), unit(2), unit(3)))).get)
    println(run(es)(parMap(List(1, 2, 3, 4, 5))(_ + 100)).get)
    println(run(es)(parFilter(List(1, 2, 3, 4, 5))(_ % 2 == 0)).get)
    println(run(es)(map3(unit(1), unit(2), unit(3))(_ + _ + _)).get)
    println(run(es)(map4(unit(1), unit(2), unit(3), unit(4))(_ + _ + _ + _)).get)
    println(run(es)(map5(unit(1), unit(2), unit(3), unit(4), unit(5))(_ + _ + _ + _ + _)).get)
    println(equal(es)(unit(1), unit(1)))
    println(equal(es)(unit(1), unit(2)))

    //    val a = lazyUnit(42 + 1)
    //    val S = Executors.newFixedThreadPool(1)
    //    println(Par.equal(S)(a, fork(a)))

  }

}


