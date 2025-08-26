package il.co.dotcore.julian.fpinscala2023.ch02

class Tests extends munit.FunSuite {
  import MyProgram.*

  test("fib") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(6) == 5)
  }

  test("isSorted") {
    assert(isSorted(Array(1, 2, 3), _ > _))
    assert(isSorted(Array(1, 2, 1), _ > _) == false)
    assert(isSorted(Array(3, 2, 1), _ < _))
    assert(isSorted(Array(1, 2, 3), _ < _) == false)
  }
}
