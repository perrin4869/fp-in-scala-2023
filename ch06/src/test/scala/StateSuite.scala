package il.co.dotcore.julian.fpinscala2023.ch06

class StateSuite extends munit.FunSuite {
  import fpinscala.state.RNG
  import fpinscala.state.SimpleRNG
  import fpinscala.state.State

  // ex10
  test("unit") {
    val (n, _) = State.unit[RNG, Int](10).run(SimpleRNG(10878))
    assert(n == 10)
  }

  // ex10
  test("map") {
    val (n, _) = State.unit[RNG, Int](10).map(_ + 1).run(SimpleRNG(10878))
    assert(n == 11)
  }

  // ex10
  test("map2") {
    val (n, _) = State.unit(1).map2(State.unit(3))(_ + _).run(SimpleRNG(10))
    assert(n == 4)
  }

  // ex10
  test("flatMap") {
    val (n, _) = State.unit(1).flatMap(_ => State.unit(2)).run(SimpleRNG(10))
    assert(n == 2)
  }

  // ex10
  test("sequence") {
    val (l, _) = State.sequence(List.fill(5)(State.unit(1))).run(SimpleRNG(10))
    assert(l == List(1, 1, 1, 1, 1))
  }
}
