import fpinscala.errorhandling.*
import Option.*

val abs0: Option[Double] => Option[Double] =
  lift(math.abs)

val ex1 = abs0(Some(-1.0))
println(ex1)
