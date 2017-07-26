import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.instances.double._
import cats.syntax.semigroup._


def add[A: Monoid](items: List[A]): A =
  items.foldLeft(Monoid[A].empty)(_ |+| _)

add(List(1,2,3))
add(List(Some(1), None))
//add(List(Some(1), Some(2)))

case class Order(totalCost: Double, quantity: Double)
implicit val orderMonoid : Monoid[Order] = new Monoid[Order] {
  override def empty: Order = Order(0,0)
  override def combine(x: Order, y: Order): Order = Order(
    totalCost = x.totalCost |+| y.totalCost,
    quantity = y.quantity |+| x.quantity)
}

add(List(Order(5, 1), Order(5, 2)))