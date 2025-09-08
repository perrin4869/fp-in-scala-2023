import fpinscala.laziness.*

val ones: LazyList[Int] = LazyList.cons(1, ones)
println(ones.take(5).toList)
println(ones.exists(_ % 2 != 0))
println(ones.map(_ + 1).exists(_ % 2 == 0))
println(ones.takeWhile(_ == 1))
println(ones.forAll(_ != 1))
