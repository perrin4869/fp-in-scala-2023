import fpinscala.state.SimpleRNG

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
println(s"n1: $n1, rng2: $rng2")
val (n2, rng3) = rng2.nextInt
println(s"n2: $n2, rng3: $rng3")
