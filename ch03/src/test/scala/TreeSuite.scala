package il.co.dotcore.julian.fpinscala2023.ch03

class TreeSuite extends munit.FunSuite {
  import fpinscala.datastructures.*
  import Tree.*
  // ex 3.25
  test("Tree[Int].maximum") {
    assert(Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)).maximum == 6)
    assert(Branch(Branch(Leaf(6), Leaf(4)), Leaf(5)).maximum == 6)
    assert(Branch(Branch(Leaf(4), Leaf(6)), Leaf(5)).maximum == 6)
  }

  // ex 3.26
  test("Tree[Int].depth") {
    assert(Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)).depth == 2)
    assert(Branch(Leaf(2), Leaf(3)).depth == 1)
    assert(Leaf(2).depth == 0)
    assert(
      Branch(Branch(Leaf(6), Branch(Leaf(4), Leaf(4))), Leaf(5)).depth == 3
    )
  }

  // ex 3.27
  test("Tree.map") {
    assert(
      Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)).map(_ + 1) == Branch(
        Branch(Leaf(6), Leaf(5)),
        Leaf(7)
      )
    )
  }

  // ex 3.28
  test("Tree.foldRight") {
    import Tree.FoldRight.*
    assert(Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)).maximum == 6)
  }

  test("Tree.fold") {
    import Tree.AnswersKey.maximum
    assert(Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)).maximum == 6)

    assert(Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)).depthViaFold == 2)
    assert(Branch(Leaf(2), Leaf(3)).depthViaFold == 1)
    assert(Leaf(2).depthViaFold == 0)
    assert(
      Branch(
        Branch(Leaf(6), Branch(Leaf(4), Leaf(4))),
        Leaf(5)
      ).depthViaFold == 3
    )

    assert(
      Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)).mapViaFold(_ + 1) == Branch(
        Branch(Leaf(6), Leaf(5)),
        Leaf(7)
      )
    )
  }
}
