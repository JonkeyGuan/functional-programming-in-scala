package rng

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(rng)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, ls: List[Int]): (List[Int], RNG) = {
      if (count == 0) (ls, rng)
      else {
        val (i, r) = rng.nextInt
        loop(count - 1, r, i :: ls)
      }
    }
    loop(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleByMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rnga) = ra(rng)
        val (b, rngb) = rb(rnga)
        (f(a, b), rngb)
      }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsBySequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    sng =>
      {
        val (a, s1) = f(sng)
        g(a)(s1)
      }
  }

  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

}

object Run {

  def main(args: Array[String]): Unit = {
    import RNG._
    val rng = Simple(System.currentTimeMillis())
    println("rng: " + rng)

    println("nonNegativeInt: " + nonNegativeInt(rng))
    println("nonNegativeInt: " + nonNegativeInt(rng))

    println("double: " + double(rng))
    println("double: " + double(rng))

    println("intDouble: " + intDouble(rng))
    println("intDouble: " + intDouble(rng))

    println("doubleInt: " + doubleInt(rng))
    println("doubleInt: " + doubleInt(rng))

    println("double3: " + double3(rng))
    println("double3: " + double3(rng))

    println("ints: " + ints(5)(rng))
    println("ints: " + ints(5)(rng))

    println("nonNegativeEven: " + nonNegativeEven(rng))
    println("nonNegativeEven: " + nonNegativeEven(rng))

    println("doubleByMap: " + doubleByMap(rng))
    println("doubleByMap: " + doubleByMap(rng))

    println("randIntDouble: " + randIntDouble(rng))
    println("randIntDouble: " + randIntDouble(rng))

    println("randDoubleInt" + randDoubleInt(rng))
    println("randDoubleInt" + randDoubleInt(rng))

    println("intsBySequence: " + intsBySequence(5)(rng))
    println("intsBySequence: " + intsBySequence(5)(rng))

  }

}
