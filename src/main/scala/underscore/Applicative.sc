case class User(name: String, age: Int)

import cats.Apply
import cats.data.Validated
import cats.syntax.either._

type FormData = Map[String, String]
type ErrorsOr[A] = Either[List[String], A]
type AllErrorsOr[A] = Validated[List[String], A]

def getValue(name: String)(data: FormData): ErrorsOr[String] =
  data.get(name).toRight(List(s"$name field not specified"))

type NumFmtExn = NumberFormatException
def parseInt(name: String)(data: String): ErrorsOr[Int] =
  Right(data).
    flatMap(s => Either.catchOnly[NumFmtExn](s.toInt)).
    leftMap(_ => List(s"$name must be an integer"))

def nonBlank(name: String)(data: String): ErrorsOr[String] =
  Right(data).
    ensure(List(s"$name cannot be blank"))(_.nonEmpty)

def nonNegative(name: String)(data: Int): ErrorsOr[Int] =
  Right(data).
    ensure(List(s"$name must be non-negative"))(_ >= 0)

def readName(data: FormData): ErrorsOr[String] =
  getValue("name")(data).
    flatMap(nonBlank("name"))

def readAge(data: FormData): ErrorsOr[Int] =
  getValue("age")(data).
    flatMap(nonBlank("age")).
    flatMap(parseInt("age")).
    flatMap(nonNegative("age"))

import cats.instances.list._
import cats.syntax.cartesian._

def user(data: FormData) = (
  readName(data).toValidated |@|
  readAge(data).toValidated
).map(User.apply)

user(Map("name" -> "Dave", "age" -> "37"))

Apply

