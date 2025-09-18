package fpinscala.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

object RNG:
  // ex01
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    (
      if n < 0 then -(n + 1) // -Int.MaxValue = Int.MinValue + 1
      else n,
      rng2
    )

  // ex02
  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    (
      n.toDouble / (Int.MaxValue.toDouble + 1.0),
      rng2
    )

  // ex03
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, r) = rng.nextInt
    val (d, r2) = double(r)
    ((n, d), r2)

  // ex03
  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, r) = double(rng)
    val (n, r2) = r.nextInt
    ((d, n), r2)

  // ex03
  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  // ex04
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count < 1 then (Nil, rng)
    else
      val (n, r) = rng.nextInt
      (n :: ints(count - 1)(r)._1, r)

  def int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  object Map:
    // ex05
    def double: Rand[Double] =
      map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  // ex06
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r => {
      val (a, r2) = ra(r)
      val (b, r3) = rb(r2)
      (f(a, b), r3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // ex07
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil))((r, r0) => map2(r, r0)(_ :: _))

  object Sequence:
    def ints(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if i + (n - 1) - mod >= 0 then (mod, rng2)
      else nonNegativeLessThan(n)(rng2)

  // ex08
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, r2) = r(rng)
      f(a)(r2)

  object FlatMap:
    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt)(i => {
        val mod = i % n
        if i + (n - 1) - mod >= 0 then unit(mod)
        else nonNegativeLessThan(n)
      })

    // ex09
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    // ex09
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))

  object AnswersKey:
    def doubleInt(rng: RNG): ((Double, Int), RNG) =
      val ((n, d), r) = intDouble(rng)
      ((d, n), r)

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
        if count <= 0 then (xs, r)
        else
          val (x, r2) = r.nextInt
          go(count - 1, r2, x :: xs)
      go(count, rng, Nil)
