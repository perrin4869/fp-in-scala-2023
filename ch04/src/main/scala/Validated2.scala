package fpinscala.errorhandling

import scala.util.control.NonFatal

enum Validated2[+E, +A]:
  case Valid(get: A)
  case Invalid(error: E)

  def map[B](f: A => B): Validated2[E, B] =
    this match
      case Valid(a)    => Valid(f(a))
      case Invalid(es) => Invalid(es)

  def map2[EE >: E, B, C](b: Validated2[EE, B])(f: (A, B) => C)(
      combineErrors: (EE, EE) => EE
  ): Validated2[EE, C] =
    (this, b) match
      case (Valid(aa), Valid(bb))     => Valid(f(aa, bb))
      case (Invalid(e), Valid(_))     => Invalid(e)
      case (Valid(_), Invalid(e))     => Invalid(e)
      case (Invalid(e1), Invalid(e2)) => Invalid(combineErrors(e1, e2))

object Validated2:
  def traverse[E, A, B](
      as: List[A],
      f: A => Validated2[E, B],
      combineErrors: (E, E) => E
  ): Validated2[E, List[B]] =
    as.foldRight(Valid(Nil): Validated2[E, List[B]])((a, acc) =>
      f(a).map2(acc)(_ :: _)(combineErrors)
    )

  def sequence[E, A](
      vs: List[Validated2[E, A]],
      combineErrors: (E, E) => E
  ): Validated2[E, List[A]] = traverse(vs, identity, combineErrors)
