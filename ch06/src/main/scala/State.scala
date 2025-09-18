package fpinscala.state

opaque type State[S, +A] = S => (A, S)
object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  type Rand[A] = State[RNG, A]

  // ex10
  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  // ex10
  extension [S, A](underlying: State[S, A])
    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, s2) = underlying(s)
        f(a)(s2)
      }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s => {
        val (a, s2) = underlying(s)
        val (b, s3) = sb(s2)
        (f(a, b), s3)
      }

  // ex10
  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight(unit(Nil))((r, r0) => r.map2(r0)(_ :: _))

  object AnswersKey:
    extension [S, A](underlying: State[S, A])
      def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        for
          a <- underlying
          b <- sb
        yield f(a, b)
