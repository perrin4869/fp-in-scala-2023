import fpinscala.errorhandling.*
import Option.*

/** Top secret formula for computing an annual car insurance premium from two
  * key factors.
  */
def insuranceRateQuote(age: Int, numbefOfSpeedingTickets: Int): Double = ???

def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String
): Option[Double] =
  val optAge: Option[Int] = toIntOption(age)
  val optTickets: Option[Int] = toIntOption(numberOfSpeedingTickets)
  map2(optAge, optTickets)(insuranceRateQuote)
