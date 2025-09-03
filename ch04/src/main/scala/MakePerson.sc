import fpinscala.errorhandling.Either
import Either.*

case class Name private (value: String)
object Name:
  def apply(name: String): Either[String, Name] =
    if name == "" || name == null then Left("Name is empty.")
    else Right(new Name(name))

case class Age private (value: Int)
object Age:
  def apply(age: Int): Either[String, Age] =
    if age < 0 then Left("Age is out of range.")
    else Right(new Age(age))

case class Person(name: Name, age: Age)
object Person:
  def make(name: String, age: Int): Either[String, Person] =
    Name(name).map2(Age(age))(Person(_, _))

  def makeBoth(name: String, age: Int): Either[List[String], Person] =
    Either.map2Both(Name(name), Age(age), Person(_, _))

println(Person.make("perrin4869", 36))
println(Person.make("", 36))
println(Person.make("perrin4869", -1))
println(Person.make("", -1))
println(Person.make(null, 36))

println(Person.makeBoth("", -1))

val p1 = Person.makeBoth("Curry", 34)
val p2 = Person.makeBoth("Howard", 44)
// the point is the inferred type having nested lists:
val pair: Either[List[List[String]], (Person, Person)] =
  map2Both(p1, p2, (_, _))

// the point is the inferred type having nested lists:
val pairAll: Either[List[String], (Person, Person)] =
  map2All(p1, p2, (_, _))
