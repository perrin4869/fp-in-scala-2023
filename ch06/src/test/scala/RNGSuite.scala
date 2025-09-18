package il.co.dotcore.julian.fpinscala2023.ch06

class RNGSuite extends munit.FunSuite {
  import fpinscala.state.*
  import RNG.*

  // ex01
  test("nonNegativeInt") {
    assert(SimpleRNG(10878).nextInt._1 == -109668761)
    assert(nonNegativeInt(SimpleRNG(10878))._1 == 109668760)
  }

  // ex02
  test("double") {
    val (d, _) = double(SimpleRNG(10878))
    assert(d == 0.0510684959590435)
  }

  // ex03
  test("intDouble, doubleInt, double3") {
    val r = SimpleRNG(10878)
    val ((n, d), r1) = intDouble(r)
    assert(n == -109668761)
    assert(d == 0.9370457120239735)
    val ((d2, n2), r2) = doubleInt(r1)
    assert(d2 == 0.5246234084479511)
    assert(n2 == -559258828)
    val ((d3, d4, d5), _) = double3(r2)
    assert(d3 == 0.6614972185343504)
    assert(d4 == 0.4501727558672428)
    assert(d5 == 0.9000771911814809)
  }

  // ex04
  test("ints") {
    val (l, _) = ints(5)(SimpleRNG(10))
    assert(l == List(3847489, 1334288366, 1486862010, 711662464, -1453296530))
  }

  // ex05
  test("map") {
    val (d, _) = Map.double(SimpleRNG(10878))
    assert(d == 0.0510684959590435)
  }

  // ex06
  test("map2") {
    val (n, r) = map2(unit(1), unit(3))(_ + _)(SimpleRNG(10))
    assert(n == 4)
  }

  // ex07
  test("sequence") {
    val (l, _) = Sequence.ints(5)(SimpleRNG(10))
    assert(l == List(3847489, 1334288366, 1486862010, 711662464, -1453296530))
  }

  // ex08
  test("flatMap") {
    val (l, _) = FlatMap.nonNegativeLessThan(10)(SimpleRNG(12))
    assert(l == 6)
  }

  // ex09
  test("FlatMap.map") {
    val (d, _) = int(SimpleRNG(10878))
    val (d1, _) = FlatMap.map(int)(_ + 1)(SimpleRNG(10878))
    assert(d + 1 == d1)
  }

  // ex09
  test("FlatMap.map2") {
    val (n, r) = FlatMap.map2(unit(1), unit(3))(_ + _)(SimpleRNG(10))
    assert(n == 4)
  }
}
