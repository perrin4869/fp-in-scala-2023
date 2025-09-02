package fpinscala.errorhandling

enum Option[+A]:
  case Some(get: A)
  case None

  // ex01
  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None    => None

  // ex01
  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  // ex01
  def getOrElse[B >: A](b: => B): B = this match
    case Some(a) => a
    case None    => b

  // ex01
  def orElse[B >: A](b: => Option[B]): Option[B] =
    this.map(a => Some(a)).getOrElse(b)

  // ex01
  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if f(a) then Some(a) else None)

object Option:
  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)

  def toIntOption(s: String): Option[Int] =
    try Some(s.toInt)
    catch case _: NumberFormatException => None

  // ex03
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  // ex04
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case head :: next => head.flatMap(a => sequence(next).map(a :: _))
      case Nil          => Some(Nil)

  // ex05
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match
      case head :: next =>
        f(head).flatMap(b => traverse(next)(f).map(b :: _))
      case Nil => Some(Nil)

  object AnswersKey:
    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      as.foldRight(Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      as.foldRight(Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))
