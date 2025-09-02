package fpinscala.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  // ex 3.26
  def depth: Int =
    this match
      case Leaf(value)         => 0
      case Branch(left, right) => (left.depth + 1).max(right.depth + 1)

  // ex 3.27
  def map[B](f: A => B): Tree[B] = this match
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  // ex 3.28
  def foldRight[B](acc: B, f: (A, B) => B): B = this match
    case Leaf(value)         => f(value, acc)
    case Branch(left, right) => left.foldRight(right.foldRight(acc, f), f)
  def foldLeft[B](acc: B, f: (A, B) => B): B = this match
    case Leaf(value)         => f(value, acc)
    case Branch(left, right) => right.foldLeft(left.foldLeft(acc, f), f)

  // ex 3.28
  def sizeViaFoldRight: Int = foldRight(0, (_, acc) => 1 + acc)

  import Tree.AnswersKey.fold
  def sizeViaFold: Int = this.fold((_) => 1, 1 + _ + _)
  def depthViaFold: Int = this.fold((_) => 0, (l, r) => (l + 1).max(r + 1))
  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), Branch(_, _))

object Tree:
  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  def firstPositive(t: Tree[Int]): Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = firstPositive(l)
      if lpos > 0 then lpos else firstPositive(r)

  // the extension method and the regular method cannot share the same name
  extension (t: Tree[Int])
    def firstPositiveExt: Int = t match
      case Leaf(i) => i
      case Branch(l, r) =>
        val lpos = l.firstPositiveExt
        if lpos > 0 then lpos else r.firstPositiveExt

  // ex 3.25
  extension (t: Tree[Int])
    def maximum: Int =
      t match
        case Leaf(value)         => value
        case Branch(left, right) => left.maximum.max(right.maximum)

  object FoldRight:
    // ex 3.28
    extension (t: Tree[Int])
      def maximum: Int = t.foldRight(Int.MinValue, (n, acc) => n.max(acc))

  object AnswersKey:
    import fpinscala.datastructures.Tree.*

    extension [A](t: Tree[A])
      def fold[B](f: A => B, g: (B, B) => B): B = t match
        case Leaf(a)      => f(a)
        case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

    extension (t: Tree[Int]) def maximum: Int = t.fold(n => n, _ max _)
