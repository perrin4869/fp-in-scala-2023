package il.co.dotcore.julian.fpinscala2023.ch02

// A comment!
/* Another comment */
/** A documentation comment */
object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  @main def printAbs: Unit =
    println(formatAbs(-42))

  def factorial(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, n * acc)

    go(n, 1)

  def fib(n: Int): Int =
    @annotation.tailrec
    def go(prev: Int, cur: Int, c: Int): Int =
      if c == n then cur
      else go(cur, prev + cur, c + 1)

    if n == 0 then 0
    else if n == 1 then 1
    else go(0, 1, 2)

  def fibAnswers(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, cur: Int, next: Int): Int =
      if n <= 0 then cur
      else go(n - 1, next, cur + next)

    go(n, 0, 1)

  private def formatFactorial(n: Int) =
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  @main def printAbsAndFactorial: Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))

  def formatResult(name: String, n: Int, f: Int => Int) =
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))

  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)

    loop(0)

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if n >= as.length then true
      else if gt(as(n), as(n - 1)) then loop(n + 1)
      else false

    loop(1)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
