package fpinscala.datastructures

import scala.annotation.targetName

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int = ints match
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def tail[A](l: List[A]): List[A] = l match
    case Nil        => sys.error("message")
    case Cons(_, t) => t

  def setHead[A](h: A, l: List[A]): List[A] = l match
    case Nil        => sys.error("message")
    case Cons(_, t) => Cons(h, t)

  // ex 3.4
  @annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    if n > 0 then
      as match
        case Nil        => Nil
        case Cons(h, t) => drop(t, n - 1)
    else as

  // ex 3.5
  @annotation.tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil        => Nil
      case Cons(h, t) => if f(h) then dropWhile(t, f) else as

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def init[A](as: List[A]): List[A] = as match
    case Nil              => Nil
    case Cons(head, Nil)  => Nil
    case Cons(head, tail) => Cons(head, init(tail))

  // ex 3.10 - not stsack safe
  // @annotation.tailrec
  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  object FoldRight:
    def sum(ns: List[Int]) =
      foldRight(ns, 0, (x, y) => x + y)

    def product(ns: List[Double]) =
      foldRight(ns, 1.0, _ * _)

    // ex 3.14
    def append[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2, Cons(_, _))

  // ex 3.9
  def length[A](as: List[A]) =
    foldRight(as, 0, (_, acc) => acc + 1)

  // ex 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil         => acc
      case Cons(a, as) => foldLeft(as, f(acc, a), f)

  // ex 3.11
  object FoldLeft:
    def sum(ns: List[Int]) =
      foldLeft(ns, 0, _ + _)

    def product(ns: List[Double]) =
      foldLeft(ns, 1.0, _ * _)

    def length(ns: List[Double]) =
      foldLeft(ns, 0, (acc, _) => acc + 1)

    // ex 3.13
    def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
      foldLeft(reverse(as), acc, (b, a) => f(a, b))

  // ex 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (acc, a) => Cons(a, acc))

  // ex 3.15
  def concat[A](aa: List[List[A]]): List[A] =
    foldRight(aa, Nil, append(_, _))

  // ex 3.16
  def increaseByOne(ns: List[Int]): List[Int] =
    ns match
      case Nil              => Nil
      case Cons(head, tail) => Cons(head + 1, increaseByOne(tail))

  // ex 3.17
  def doubleToString(ds: List[Double]): List[String] =
    ds match
      case Nil              => Nil
      case Cons(head, tail) => Cons(head.toString, doubleToString(tail))

  // ex 3.18
  def map[A, B](as: List[A], f: A => B): List[B] =
    as match
      case Nil              => Nil
      case Cons(head, tail) => Cons(f(head), map(tail, f))

  // ex 3.19
  def filter[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil                         => Nil
      case Cons(head, tail) if f(head) => Cons(head, filter(tail, f))
      case Cons(head, tail)            => filter(tail, f)

  // ex 3.20
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil, (a, bs) => append(f(a), bs))

  // ex 3.21
  object FlatMap:
    def filter[A](as: List[A], f: A => Boolean): List[A] =
      flatMap(as, a => if f(a) then List(a) else Nil)

  // ex 3.22
  def addition(n1: List[Int], n2: List[Int]): List[Int] =
    (n1, n2) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addition(t1, t2))
      case _                            => Nil

  // ex 3.23
  def combine[A](a1: List[A], a2: List[A], f: (A, A) => A): List[A] =
    (a1, a2) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), combine(t1, t2, f))
      case _                            => Nil

  // ex 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @annotation.tailrec
    def check(sup: List[A], sub: List[A]): Boolean =
      (sup, sub) match
        case (_, Nil)                     => true
        case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && check(t1, t2)
        case _                            => false

    sup match
      case Nil        => false
      case Cons(h, t) =>
        if check(sup, sub) then true else hasSubsequence(t, sub)

  object AnswersKey:
    import fpinscala.datastructures.List.*

    @annotation.tailrec
    def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
      as match
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _                  => as

    def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B) =
      foldLeft(as, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

    def foldLeftViaFoldRight[A, B](as: List[A], acc: B, f: (A, B) => B) =
      foldRight(as, (b: B) => b, (a, g) => b => g(f(a, b)))(acc)

    // ex 3.16
    def incrementEach(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int], (i, acc) => Cons(i + 1, acc))

    // ex 3.17
    def doubleToString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String], (d, acc) => Cons(d.toString, acc))

    // ex 3.18
    def map[A, B](as: List[A], f: A => B): List[B] =
      foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))

    // ex 3.19
    def filter[A](as: List[A], f: A => Boolean): List[A] =
      foldRight(
        as,
        Nil: List[A],
        (a, acc) => if f(a) then Cons(a, acc) else acc
      )

    // ex 3.23
    def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
      (a, b) match
        case (Nil, _)                     => Nil
        case (_, Nil)                     => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

    // ex 3.23
    def zipWithStackSafe[A, B, C](
        a: List[A],
        b: List[B],
        f: (A, B) => C
    ): List[C] =
      @annotation.tailrec
      def loop(a: List[A], b: List[B], acc: List[C]): List[C] =
        (a, b) match
          case (Nil, _)                     => Nil
          case (_, Nil)                     => Nil
          case (Cons(h1, t1), Cons(h2, t2)) =>
            loop(t1, t2, Cons(f(h1, h2), acc))
      reverse(loop(a, b, Nil))

    // ex 3.24
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match
      case (_, Nil)                              => true
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _                                     => false

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h, t)                => hasSubsequence(t, sub)
