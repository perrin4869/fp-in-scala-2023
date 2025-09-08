package fpinscala.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, _) => Some(h())

  // ex01
  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  // ex02
  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _                   => Empty

  // ex02
  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) => if n > 1 then t().drop(n - 1) else t()
    case Empty      => Empty

  // ex03
  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _                    => Empty

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _          => acc

  def exists(p: A => Boolean): Boolean =
    this.foldRight(false)((a, b) => p(a) || b)

  // ex04
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  import LazyList.*

  // ex05
  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    this.foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  // ex06
  def headOptionViaFoldRight: Option[A] =
    this.foldRight(None)((a, b) => Some(a))

  // ex07
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, b) => cons(f(a), b))

  // ex07
  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if (f(a)) then cons(a, b) else b)

  // ex07
  infix def append[B >: A](l: => LazyList[B]): LazyList[B] =
    foldRight(l)((a, b) => cons(a, b))

  // ex07
  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)(f(_) append _)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOptionViaFoldRight

  // ex13
  object ViaUnfoldMethods:
    def map[B](f: A => B): LazyList[B] =
      unfold(LazyList.this):
        case Cons(h, t) => Some((f(h()), t()))
        case Empty      => None

    def take(n: Int): LazyList[A] =
      unfold((LazyList.this, n)):
        case (Cons(h, _), 1)          => Some((h(), (empty, 0)))
        case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
        case _                        => None

    def takeWhile(p: A => Boolean): LazyList[A] =
      unfold(LazyList.this):
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _                    => None

  // ex13
  def zipWith[B](that: LazyList[B]): LazyList[(A, B)] =
    unfold((LazyList.this), (that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
      case _                            => None

  // ex13
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((LazyList.this), (that)):
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), l: Empty.type) =>
        Some(((Some(h1()), None), (t1(), l)))
      case (l: Empty.type, Cons(h2, t2)) =>
        Some(((None, Some(h2())), (l, t2())))
      case _ => None

  // ex14
  def startsWith[B >: A](prefix: LazyList[B]): Boolean =
    zipAll(prefix).foldRight(false):
      case ((Some(a1), Some(a2)), b) => a1 == a2 && b
      case ((Some(_), None), _)      => true
      case _                         => false

  // ex15
  def tails: LazyList[LazyList[A]] =
    (unfold(this):
      case Cons(h, t) => Some(cons(h(), t()), t())
      case _          => None
    )
    .append(LazyList(empty))

  def hasSubsequence[B >: A](l: LazyList[B]): Boolean =
    tails.exists(_.startsWith(l))

  // ex16
  // this solution is O(n^2), because unfold traverses the list left-associated
  // in order to do O(n) we need foldRight, see AnswersKey.scanRight
  def scanRight[B](b: B)(f: (A, B) => B): LazyList[B] =
    unfold(this):
      case l @ Cons(_, t) => Some(l.foldRight(b)(f(_, _)), t())
      case _              => None
    .append(LazyList(empty.foldRight(b)(f(_, _))))

  object AnswersKey:
    // ex02
    def take(n: Int): LazyList[A] = LazyList.this match
      case Cons(h, t) if n > 1  => cons(h(), t().AnswersKey.take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _                    => empty

    // ex02
    @annotation.tailrec
    def drop(n: Int): LazyList[A] = LazyList.this match
      case Cons(h, t) if n > 0 => t().AnswersKey.drop(n - 1)
      case _                   => LazyList.this

    // ex03
    def takeWhile(p: A => Boolean): LazyList[A] = LazyList.this match
      case Cons(h, t) if p(h()) => cons(h(), t().AnswersKey.takeWhile(p))
      case _                    => empty

    // ex13
    def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
      unfold((LazyList.this), (that)):
        case (Empty, Empty) => None
        case (Cons(h1, t1), Empty) =>
          Some((Some(h1()) -> None) -> (t1() -> Empty))
        case (Empty, Cons(h2, t2)) =>
          Some((None -> Some(h2())) -> (Empty -> t2()))
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))

    // ex14
    def startsWith[B >: A](prefix: LazyList[B]): Boolean =
      zipAll(prefix).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

    // ex15
    def tails: LazyList[LazyList[A]] =
      unfold(LazyList.this):
        case Empty          => None
        case l @ Cons(_, t) => Some((l, t()))
      .append(LazyList(empty))

    def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
      foldRight(init -> LazyList(init)): (a, b0) =>
        lazy val b1 = b0
        val b2 = f(a, b1(0))
        (b2, cons(b2, b1(1)))
      .apply(1)

object LazyList:
  def cons[A](
      hd: => A,
      tl: => LazyList[A]
  ): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  // ex08
  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  // ex09
  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  // ex10
  def fibs: LazyList[Int] =
    def loop(prev: Int, cur: Int): LazyList[Int] =
      cons(cur, loop(cur, prev + cur))
    cons(0, loop(0, 1))

  // ex11
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state)
      .map { case (a, s) =>
        cons(a, unfold(s)(f))
      }
      .getOrElse(empty)

  object ViaUnfold:
    def fibs: LazyList[Int] =
      unfold((0, 1)):
        case (current, next) =>
          Some((current, (next, current + next)))

    def from(n: Int): LazyList[Int] =
      unfold(n)(n => Some((n, n + 1)))

    def continually[A](a: A): LazyList[A] =
      unfold(a)(a => Some((a, a)))

    def ones: LazyList[Int] =
      continually(1)

    object AnswersKey:
      def continually[A](a: A): LazyList[A] =
        unfold(())(_ => Some((a, ())))

      def ones: LazyList[Int] =
        unfold(())(_ => Some((1, ())))

  object AnswersKeyCompanionObject:
    val fibs: LazyList[Int] =
      def go(current: Int, next: Int): LazyList[Int] =
        cons(current, go(next, current + next))
      go(0, 1)

    def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
      f(state) match
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None         => empty
