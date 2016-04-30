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
}

object Run {

  def main(args: Array[String]): Unit = {
    import RNG._
    val rng = Simple(System.currentTimeMillis())
    println(rng)

    println(nonNegativeInt(rng))
    println(nonNegativeInt(rng))

    println(double(rng))
    println(double(rng))

    println(intDouble(rng))
    println(intDouble(rng))

    println(doubleInt(rng))
    println(doubleInt(rng))

    println(double3(rng))
    println(double3(rng))

    println(ints(5)(rng))
    println(ints(5)(rng))

    println(nonNegativeEven(rng))
    println(nonNegativeEven(rng))

    println(doubleByMap(rng))
    println(doubleByMap(rng))
  }

}
