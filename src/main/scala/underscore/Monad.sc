
import scala.concurrent.{Await, Future}
import scala.language.higherKinds



trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))
}

type Id[A] = A
def pure[A](a: A): Id[A] = a
def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)

//2.11
val either2: Either[String, Int] = Right(321)
either2.left.flatMap(s => Left(s + "!"))

import cats.syntax.either._
Either.catchOnly[NumberFormatException]("foo".toInt)
Either.fromOption[String, Int](Some(2), "Badness")
Either.catchNonFatal(sys.error("Badness"))

import cats.Eval

def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
  as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil => acc
  }

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRightEval(as, Eval.now(acc))((a, b) => {
    b.map(fn(a, _))
  }).value
foldRight((1 to 100000).toList, 0)(_ + _)

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._
def slowly[A](body: => A) = try body finally Thread.sleep(100)

type Logged[A] = Writer[Vector[String], A]
41.pure[Logged]
42.writer(Vector.empty[String]).map(_ + 1)
Vector("Message").tell
//factorial(5)

def factorial(n: Int): Logged[Int] =
  for {
    ans <- if (n == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial(n - 1).map(_ * n))
    }
    _ <- Vector(s"fact $n $ans").tell
  } yield ans

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

Await.result(Future.sequence(Vector(
  Future(factorial(5).run),
  Future(factorial(5).run)
)), 5.seconds)



