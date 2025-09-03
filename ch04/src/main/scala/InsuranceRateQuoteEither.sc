import fpinscala.errorhandling.*
import Either.*

/** Top secret formula for computing an annual car insurance premium from two
  * key factors.
  */
def insuranceRateQuote(age: Int, numbefOfSpeedingTickets: Int): Double = ???

def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String
): Either[Throwable, Double] =
  for {
    a <- Either.catchNonFatal(age.toInt)
    tickets <- Either.catchNonFatal(numberOfSpeedingTickets.toInt)
  } yield insuranceRateQuote(a, tickets)
