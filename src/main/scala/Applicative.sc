import cats._
import cats.data.OptionT
import cats.implicits._

(Applicative[List] compose Applicative[Option]).pure(1)

Monad[List].flatMap(List(1, 2, 3))(x ⇒ List(x, x))

Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy"))
Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4))

OptionT.pure[List, Int](42)

val lazyResult =
  Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) ⇒ Later(x + rest.value))
lazyResult.value
