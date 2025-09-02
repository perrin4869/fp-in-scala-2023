package il.co.dotcore.julian.fpinscala2023.ch04

class OptionSuite extends munit.FunSuite {
  import fpinscala.errorhandling.*
  import Option.*

  // ex01
  test("Option.map") {
    assert(Some(3).map(_ + 1) == Some(4))
    assert((None: Option[Int]).map(_ + 1) == None)
  }

  // ex01
  test("Option.flatMap") {
    assert(Some(3).flatMap(n => Some(n + 1)) == Some(4))
    assert((None: Option[Int]).flatMap(_ => Some(1)) == None)
    assert(Some(3).flatMap(_ => None) == None)
  }

  // ex01
  test("Option.getOrElse") {
    assert(Some(3).getOrElse(4) == 3)
    assert((None: Option[Int]).getOrElse(10) == 10)
  }

  // ex01
  test("Option.orElse") {
    assert(Some(3).orElse(Some(4)) == Some(3))
    assert((None: Option[Int]).orElse(Some(10)) == Some(10))
  }

  // ex01
  test("Option.filter") {
    assert(Some(3).filter(_ == 3) == Some(3))
    assert(Some(3).filter(_ > 2) == Some(3))
    assert(Some(3).filter(_ < 2) == None)
    assert((None: Option[Int]).filter(_ == 3) == None)
  }

  // ex02
  test("variance") {
    import ch04.src.main.scala.MeanOption.mean
    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    assert(variance(List(1)) == Some(0))
    assert(variance(List()) == None)
    assert(variance(List(1, 2)) == Some(0.25))
  }

  // ex03
  test("map2") {
    assert(map2(Some(1), Some(2))(_ + _) == Some(3))
    assert(map2(None: Option[Int], Some(2))(_ + _) == None)
  }

  // ex04
  test("sequence") {
    assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
    assert(sequence(List(Some(1), None)) == None)
    assert(sequence(List(None, Some(1))) == None)
  }

  // ex05
  test("traverse") {
    def parseInts(as: List[String]): Option[List[Int]] =
      traverse(as)(toIntOption)
    assert(parseInts(List("1", "2")) == Some(List(1, 2)))
    assert(parseInts(List("1", "foo")) == None)
  }
}
