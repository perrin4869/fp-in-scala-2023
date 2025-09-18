package fpinscala.state
import State.*

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

// ex11
def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  inputs.foldLeft(get[Machine].map(m => (m.coins, m.candies)))((s, i) =>
    i match
      case Input.Coin =>
        for
          _ <- s
          _ <- modify[Machine]:
            case Machine(true, candies, coins) if candies > 0 =>
              Machine(false, candies, coins + 1)
            case m => m
          m <- get
        yield (m.coins, m.candies)
      case Input.Turn =>
        for
          _ <- s
          _ <- modify[Machine]:
            case Machine(false, candies, coins) if candies > 0 =>
              Machine(true, candies - 1, coins)
            case m => m
          m <- get
        yield (m.coins, m.candies)
  )

object Machine:
  object AnswersKey:
    val update = (i: Input) =>
      (s: Machine) =>
        (i, s) match
          case (_, Machine(_, 0, _))              => s
          case (Input.Coin, Machine(false, _, _)) => s
          case (Input.Turn, Machine(true, _, _))  => s
          case (Input.Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Input.Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)

    def traverse[S, A, B](as: List[A])(
        f: A => State[S, B]
    ): State[S, List[B]] =
      as.foldRight(unit(Nil))((a, acc) => f(a).map2(acc)(_ :: _))

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      for
        // _ <- sequence(inputs.map(i => modify(update(i))))
        _ <- traverse(inputs)(i => modify(update(i)))
        s <- get
      yield (s.coins, s.candies)
