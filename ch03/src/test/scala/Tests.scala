package il.co.dotcore.julian.fpinscala2023.ch03

class Tests extends munit.FunSuite {
  import fpinscala.datastructures.*
  import List.*

  test("tail") {
    assert(tail(List(1, 2, 3)) == List(2, 3))
  }

  test("setHead") {
    assert(setHead(4, List(1, 2, 3)) == List(4, 2, 3))
  }

  test("drop") {
    assert(drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }

  test("dropWhile") {
    assert(dropWhile(List(1, 2, 3, 4, 5), n => n < 4) == List(4, 5))
    assert(AnswersKey.dropWhile(List(1, 2, 3, 4, 5), n => n < 4) == List(4, 5))
  }

  test("init") {
    assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
  }

  test("foldRight") {
    assert(
      // ex 3.8
      foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _)) == List(1, 2, 3)
    )
  }

  test("length") {
    assert(length(List(1, 2, 3, 4, 5)) == 5)
  }

  test("foldLeft") {
    assert(
      // ex 3.10
      foldLeft(List(1, 2, 3), 0, _ + _) == 6
    )

    // ex 3.11
    assert(FoldLeft.sum(List(1, 2, 3)) == 6)
    assert(FoldLeft.product(List(1, 2, 3, 4)) == 24)
    assert(FoldLeft.length(List(1, 2, 3, 4)) == 4)

  }

  // ex 3.12
  test("reverse") {
    assert(reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
  }

  // ex 3.13
  test("foldRightViaFoldLeft") {
    assert(FoldLeft.foldRight(List(1, 2, 3, 4, 5), 0, _ + _) == 15)
  }

  // ex 3.14
  test("append") {
    assert(
      FoldRight.append(List(1, 2, 3, 4, 5), List(6, 7, 8)) == List(1, 2, 3, 4,
        5, 6, 7, 8)
    )
  }

  // ex 3.15
  test("concat") {
    assert(
      concat(
        List(List(1, 2, 3, 4, 5), List(6, 7, 8), List(9, 10), List(11, 12))
      ) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    )
  }

  // ex 3.16
  test("increaseByOne") {
    assert(
      increaseByOne(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5, 6)
    )
  }

  // ex 3.17
  test("doubleToString") {
    assert(
      doubleToString(List(1.0, 2.0, 3.0, 4.0)) == List(
        "1.0",
        "2.0",
        "3.0",
        "4.0"
      )
    )
  }

  // ex 3.18
  test("map") {
    assert(
      map(List(1, 2, 3, 4, 5), _ + 1) == List(2, 3, 4, 5, 6)
    )
    assert(
      map(List(1.0, 2.0, 3.0, 4.0), _.toString) == List(
        "1.0",
        "2.0",
        "3.0",
        "4.0"
      )
    )
  }

  // ex 3.19
  test("filter") {
    assert(
      filter(List(1, 2, 3, 4, 5, 6, 7), _ % 2 == 0) == List(2, 4, 6)
    )
  }

  // ex 3.20
  test("flatMap") {
    assert(
      flatMap(List(1, 2, 3), i => List(i, i)) == List(1, 1, 2, 2, 3, 3)
    )
  }

  // ex 3.21
  test("filterViaFlatMap") {
    assert(
      FlatMap.filter(List(1, 2, 3, 4, 5, 6, 7), _ % 2 == 0) == List(2, 4, 6)
    )
  }

  // ex 3.22
  test("addition") {
    assert(addition(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  // ex 3.23
  test("combine") {
    assert(combine(List(1, 2, 3), List(4, 5, 6), _ + _) == List(5, 7, 9))
  }

  // ex 3.24
  test("hasSubsequence") {
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 4)) == false)
  }
}
