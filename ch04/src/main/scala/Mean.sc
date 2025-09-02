def mean(xs: Seq[Double]): Double =
  if xs.isEmpty then throw new ArithmeticException("mean of empty list!")
  else xs.sum / xs.length

val l = List(10.0, 30.0, 40.0)
println(s"mean of %s = %s".format(l, mean(l)))

val l0 = List()
println(s"mean of %s = %s".format(l0, mean(l0)))
