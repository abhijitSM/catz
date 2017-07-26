import cats.{Foldable, Semigroup}
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.semigroup._
import cats.syntax.cartesian._ // |@| syntax

sealed trait Check[E, A] {
  def and(that: Check[E, A]): Check[E, A] =
    And(this, that)

  def or(that: Check[E, A]): Check[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) =>
        func(a)

      case And(left, right) =>
        (left(a) |@| right(a)).map((_, _) => a)

      case Or(left, right) => left(a) match {
        case Valid(a1) => Valid(a1)
        case Invalid(e1) => right(a) match {
          case Valid(b) => Valid(b)
          case Invalid(e2) => Invalid(e1 |+| e2)
        }
      }
    }
}

final case class And[E, A](
  left: Check[E, A],
  right: Check[E, A]
) extends Check[E, A]

final case class Or[E, A](
  left: Check[E, A],
  right: Check[E, A]
) extends Check[E, A]

final case class Pure[E, A](
  func: A => Validated[E, A]
) extends Check[E, A]

Foldable
