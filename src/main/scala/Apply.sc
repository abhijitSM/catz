import cats._
import cats.implicits._

val listOpt = Apply[List] compose Apply[Option]
val plusOne = (x: Int) ⇒ x + 1
listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3)))

val addArity2 = (a: Int, b: Int) ⇒ a + b
Apply[Option].ap2(Some(addArity2))(Some(1), Some(2))

Apply[Option].map2(Some(1), Some(2))(addArity2)

Apply[Option].tuple2(Some(1), Some(2))

val option2 = Option(1) |@| Option(2)
val option3 = option2 |@| Option.empty[Int]

option2 map addArity2
option2 apWith Some(addArity2)
option2.tupled
option3.tupled