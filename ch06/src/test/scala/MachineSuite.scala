package il.co.dotcore.julian.fpinscala2023.ch06

class MachineSuite extends munit.FunSuite {
  import fpinscala.state.Machine
  import fpinscala.state.Input
  import fpinscala.state.simulateMachine

  // ex11
  test("machine") {
    val ((coins, candies), machine) = simulateMachine(
      List(
        Input.Coin,
        Input.Turn,
        Input.Coin,
        Input.Turn,
        Input.Coin,
        Input.Turn,
        Input.Coin,
        Input.Turn
      )
    ).run(Machine(true, 5, 10))

    assert(coins == 14)
    assert(candies == 1)
    assert(machine.locked == true)
  }

  // ex11
  test("AnswersKey.machine") {
    val ((coins, candies), machine) = Machine.AnswersKey
      .simulateMachine(
        List(
          Input.Coin,
          Input.Turn,
          Input.Coin,
          Input.Turn,
          Input.Coin,
          Input.Turn,
          Input.Coin,
          Input.Turn
        )
      )
      .run(Machine(true, 5, 10))

    assert(coins == 14)
    assert(candies == 1)
    assert(machine.locked == true)
  }
}
