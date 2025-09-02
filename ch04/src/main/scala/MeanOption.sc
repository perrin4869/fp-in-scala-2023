import fpinscala.errorhandling.*
import Option.*

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

val l = List(10.0, 30.0, 40.0)
println(s"mean of %s = %s".format(l, mean(l)))

val l0 = List()
println(s"mean of %s = %s".format(l0, mean(l0)))
