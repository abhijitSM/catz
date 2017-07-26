import cats._
import cats.implicits._

Monoid[String].empty
Monoid[String].combineAll(List("a", "b", "c"))
Monoid[String].combineAll(List())

Monoid[Map[String, Int]].combineAll(List(Map("a" → 1, "b" → 2), Map("a" → 3)))
Monoid[Map[String, Int]].combineAll(List())

val l = List(1, 2, 3, 4, 5)
l.foldMap(i ⇒ (i, i.toString))

//https://github.com/non/kind-projector
implicit def function1Functor[In]: Functor[Function1[In, ?]] =
  new Functor[Function1[In, ?]] {
    def map[A, B](fa: In => A)(f: A => B): Function1[In, B] = fa andThen f
  }

val lenOption: Option[String] ⇒ Option[Int] = Functor[Option].lift(_.length)
lenOption(Some("Hello"))

val source = List("Cats", "is", "awesome")
val product = Functor[List].fproduct(source)(_.length).toMap

val listOpt = Functor[List] compose Functor[Option]
listOpt.map(List(Some(1), None, Some(3)))( x => x + 1)
