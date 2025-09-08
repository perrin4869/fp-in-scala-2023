package il.co.dotcore.julian.fpinscala2023.ch05

class LazyListSuite extends munit.FunSuite {
  import fpinscala.laziness.*
  import LazyList.*

  // ex01
  test("LazyList.toList") {
    assert(LazyList(1, 2, 3, 4).toList == List(1, 2, 3, 4))
  }

  // ex02
  test("LazyList.take") {
    assert(LazyList.empty.take(3).toList == List.empty)
    assert(LazyList(1, 2, 3, 4).take(2).toList == List(1, 2))
  }

  // ex02
  test("LazyList.drop") {
    assert(LazyList.empty.drop(3).toList == List.empty)
    assert(LazyList(1, 2, 3, 4).drop(2).toList == List(3, 4))
  }

  // ex03
  test("LazyList.takeWhile") {
    assert(LazyList(1, 2, 3, 4).takeWhile(n => n < 4).toList == List(1, 2, 3))
  }

  // ex04
  test("LazyList.forAll") {
    assert(LazyList(1, 2, 3, 4).forAll(n => n < 5) == true)
    assert(LazyList(1, 2, 3, 4).forAll(n => n < 3) == false)
  }

  // ex05
  test("LazyList.takeWhileViaFoldRight") {
    assert(
      LazyList(1, 2, 3, 4).takeWhileViaFoldRight(n => n < 4).toList == List(
        1,
        2,
        3
      )
    )
  }

  // ex06
  test("LazyList.headOption") {
    assert(LazyList(1, 2, 3).headOptionViaFoldRight == Some(1))
    assert(LazyList().headOptionViaFoldRight == None)
  }

  // ex07
  test("LazyList.map") {
    assert(LazyList(1, 2, 3).map(_ * 2).toList == List(2, 4, 6))
    assert(LazyList(1, 2, 3).map(_ * 2).take(2).toList == List(2, 4))
  }

  // ex07
  test("LazyList.filter") {
    assert(LazyList(1, 2, 3).filter(_ != 2).toList == List(1, 3))
  }

  // ex07
  test("LazyList.append") {
    assert(
      LazyList(1, 2, 3).append(LazyList(4, 5)).toList == List(1, 2, 3, 4, 5)
    )
    assert(
      LazyList(1, 2, 3).append(LazyList(4, 5)).take(4).toList == List(
        1,
        2,
        3,
        4
      )
    )
  }

  // ex07
  test("LazyList.flatMap") {
    assert(
      LazyList(1, 2, 3).flatMap(n => LazyList(n, n)).toList == List(1, 1, 2, 2,
        3, 3)
    )
    assert(
      LazyList(1, 2, 3).flatMap(n => LazyList(n, n)).take(3).toList == List(
        1,
        1,
        2
      )
    )
  }

  // ex08
  test("LazyList.continually") {
    assert(continually(7).take(3).toList == List(7, 7, 7))
  }

  // ex09
  test("LazyList.from") {
    assert(from(4).take(3).toList == List(4, 5, 6))
  }

  // ex10
  test("LazyList.fibs") {
    assert(fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  // ex12
  test("LazyList.ViaUnfold") {
    import LazyList.ViaUnfold.*
    assert(fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    assert(from(4).take(3).toList == List(4, 5, 6))
    assert(continually(7).take(3).toList == List(7, 7, 7))
  }

  // ex13
  test("LazyList.ViaUnfoldMethods") {
    assert(
      LazyList(1, 2, 3).ViaUnfoldMethods.map(_ * 2).toList == List(2, 4, 6)
    )
    assert(
      LazyList(1, 2, 3).ViaUnfoldMethods.map(_ * 2).take(2).toList == List(2, 4)
    )
    assert(LazyList.empty.ViaUnfoldMethods.take(3).toList == List.empty)
    assert(LazyList(1, 2, 3, 4).ViaUnfoldMethods.take(2).toList == List(1, 2))
    assert(
      LazyList(1, 2, 3, 4).ViaUnfoldMethods
        .takeWhile(n => n < 4)
        .toList == List(1, 2, 3)
    )
  }

  // ex13
  test("LazyList.zipWith") {
    assert(
      LazyList(1, 2, 3).zipWith(LazyList(4, 5, 6)).toList == List(
        (1, 4),
        (2, 5),
        (3, 6)
      )
    )
    assert(
      LazyList(1, 2).zipWith(LazyList(4, 5, 6)).toList == List(
        (1, 4),
        (2, 5)
      )
    )
    assert(
      LazyList(1, 2, 3).zipWith(LazyList(4)).toList == List(
        (1, 4)
      )
    )
  }

  // ex13
  test("LazyList.zipAll") {
    assert(
      LazyList(1, 2, 3).zipAll(LazyList(4, 5, 6)).toList == List(
        (Some(1), Some(4)),
        (Some(2), Some(5)),
        (Some(3), Some(6))
      )
    )
    assert(
      LazyList(1, 2).zipAll(LazyList(4, 5, 6)).toList == List(
        (Some(1), Some(4)),
        (Some(2), Some(5)),
        (None, Some(6))
      )
    )
    assert(
      LazyList(1, 2, 3).zipAll(LazyList(4)).toList == List(
        (Some(1), Some(4)),
        (Some(2), None),
        (Some(3), None)
      )
    )
  }

  // ex14
  test("LazyList.startsWith") {
    assert(LazyList(1, 2, 3).startsWith(LazyList(1, 2)))
    assert(LazyList(1, 2, 3).startsWith(LazyList(2, 4)) == false)
    assert(LazyList(1, 2, 3).startsWith(LazyList(1, 2, 3, 4)) == false)
  }

  // ex15
  test("LazyList.tails") {
    assert(
      LazyList(1, 2, 3).tails.toList.map(_.toList) == List(
        List(1, 2, 3),
        List(2, 3),
        List(3),
        List()
      )
    )
    assert(
      LazyList(1, 2, 3).AnswersKey.tails.toList.map(_.toList) == List(
        List(1, 2, 3),
        List(2, 3),
        List(3),
        List()
      )
    )
  }

  // ex16
  test("LazyList.scanRight") {
    assert(
      LazyList(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0)
    )
    assert(
      LazyList(1, 2, 3).AnswersKey.scanRight(0)(_ + _).toList == List(
        6,
        5,
        3,
        0
      )
    )
  }
}
