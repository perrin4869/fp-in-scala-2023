def sum(ints: IndexedSeq[Int]): Int =
  if ints.size <= 1 then ints.headOption.getOrElse(0)
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    sum(l) + sum(r)

println(sum(Vector.range(1, 10)))
