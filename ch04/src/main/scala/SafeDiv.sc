import scala.util.control.NonFatal
import fpinscala.errorhandling.*
import Either.*

def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
  try Right(x / y)
  catch case NonFatal(t) => Left(t)

println(s"%d / %d = %s".format(4, 2, safeDiv(4, 2)))
println(s"%d / %d = %s".format(1, 0, safeDiv(1, 0)))
