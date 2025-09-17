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
  def ints(counts: Int)(rng: RNG): (List[Int], RNG) =
    if counts < 1 then (Nil, rng)
    else
      val (n, r) = rng.nextInt
      (n :: ints(counts - 1)(r)._1, r)

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
