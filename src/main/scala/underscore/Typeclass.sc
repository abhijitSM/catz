/*
“Let’s define a Printable type class to work around these problems:

Define a type class Printable[A] containing a single method format. format should accept a value of type A and returns a String.

Create an object PrintableInstances containing instances of Printable for String and Int.

Define an object Printable with two generic interface methods:

format accepts a value of type A and a Printable of the corresponding type. It uses the relevant Printable to convert the A to a String.

print accepts the same parameters as format and returns Unit. It prints the A value to the console using println.”

Excerpt From: Noel Welsh and Dave Gurnell. “Advanced Scala.” iBooks. 
This material may be protected by copyright.
 */

trait Printable[A] {
  def format(value: A): String
}

object Printable {

  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)

    def print(implicit p: Printable[A]): Unit = println(value)
  }

}

case object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    override def format(value: String): String = value
  }
  implicit val intPrintable = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }
}

final case class Cat(
  name: String,
  age: Int,
  color: String
)

implicit val catPrintable = new Printable[Cat] {
  override def format(value: Cat): String = s"${value.name} is a ${value.age} year-old ${value.color} cat."
}

import Printable._
val cat = Cat(name = "caty", age = 3, color = "black")
cat.format

import cats.Show
import cats.instances.int._
import cats.syntax.show._

implicit val catShow : Show[Cat] = Show.show(value => s"${value.name} is a ${value.age} year-old ${value.color} cat.")
cat.show

trait Eqv[+A]
class OptEqv[T] extends Eqv[Option[T]]
class IntEqv extends Eqv[Int]
class SomeEqv[Z] extends Eqv[Some[Z]]

def method[T](e: Eqv[Option[T]]) = println(s"works")

method[Int](new SomeEqv[Int])

val cat1 = Cat("Garfield",   35, "orange and black")
val cat2 = Cat("Heathcliff", 30, "orange and black")
val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

import cats.Eq
import cats.instances.string._
import cats.instances.option._
import cats.syntax.eq._

implicit val catsEq : Eq[Cat] = Eq.instance((c1, c2) => {
  c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
})

cat1 === cat2
optionCat1 === optionCat2
