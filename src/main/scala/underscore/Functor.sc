import scala.annotation.tailrec

import cats.Functor

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
def leaf[A](value: A): Tree[A] = Leaf(value)

implicit val treeFunc = new Functor[Tree] {
  override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = {
    fa match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }
}

treeFunc.map(leaf(20))(_ * 2)
treeFunc.map(branch(leaf(1), leaf(2)))(_ * 2)
