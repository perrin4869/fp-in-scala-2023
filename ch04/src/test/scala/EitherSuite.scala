package il.co.dotcore.julian.fpinscala2023.ch04

class EitherSuite extends munit.FunSuite {
  import fpinscala.errorhandling.*
  import Either.*

  // ex06
  test("Either.map") {
    assert(Right(3).map(_ + 1) == Right(4))
    assert(((Left("omg"): Either[String, Int])).map(_ + 1) == Left("omg"))
  }

  // ex06
  test("Either.flatMap") {
    assert(Right(3).flatMap(n => Right(n + 1)) == Right(4))
    assert(
      (Left("omg"): Either[String, Int]).flatMap(_ => Right(1)) == Left("omg")
    )
    assert(Right(3).flatMap(_ => Left("omg")) == Left("omg"))
  }

  // ex06
  test("Either.orElse") {
    assert(Right(3).orElse(Right(4)) == Right(3))
    assert((Left("omg"): Either[String, Int]).orElse(Right(10)) == Right(10))
  }

  // ex06
  test("Either.map2") {
    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
    assert(
      (Left("omg"): Either[String, Int]).map2(Right(2))(_ + _) == Left("omg")
    )
    assert(
      Right(2).map2((Left("omg"): Either[String, Int]))(_ + _) == Left("omg")
    )
  }

  // ex07
  test("sequence") {
    assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
    assert(
      sequence(List(Right(1), (Left("omg"): Either[String, Int]))) == Left(
        "omg"
      )
    )
    assert(
      sequence(List((Left("omg"): Either[String, Int]), Right(1))) == Left(
        "omg"
      )
    )
  }

  // ex07
  test("traverse") {
    def parseInts(as: List[String]): Either[Throwable, List[Int]] =
      traverse(as)(a => Either.catchNonFatal(a.toInt))
    println(parseInts(List("1", "foo")))
    assert(parseInts(List("1", "2")) == Right(List(1, 2)))
    assert(
      parseInts(List("1", "foo")) match
        case Left(e: NumberFormatException)
            if e.getMessage() == "For input string: \"foo\"" =>
          true
        case _ => false
    )
  }
}
