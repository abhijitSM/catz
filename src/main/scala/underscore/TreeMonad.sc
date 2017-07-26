import scala.annotation.tailrec

import cats.Monad

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)

implicit val treeM = new Monad[Tree] {
  override def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] = fa match {
    case Leaf(v) => f(v)
    case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
  }

  override def pure[A](x: A): Tree[A] = leaf(x)

  override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = {
    f(a) match {
      case Leaf(Right(b)) => leaf(b)
      case Leaf(Left(aa)) => tailRecM(aa)(f)
      case Branch(left, right) => Branch(
        flatMap(left){
          case Left(l) => tailRecM(l)(f)
          case Right(b) => leaf(b)
        }, flatMap(right) {
          case Left(l) => tailRecM(l)(f)
          case Right(b) => leaf(b)
        }
      )
    }
  }
}

import cats.syntax.functor._
import cats.syntax.flatMap._

toFlatMapOps(
  branch(
    leaf(scala.runtime.BoxesRunTime.boxToInteger(100)),
    leaf(scala.runtime.BoxesRunTime.boxToInteger(200))))(treeM).
  flatMap((a: Integer) => toFlatMapOps(branch(
    leaf(scala.runtime.BoxesRunTime.boxToInteger(Integer2int(a).-(10))), // 90, 110
    leaf(scala.runtime.BoxesRunTime.boxToInteger(Integer2int(a).+(10)))))(treeM). //190,210
    flatMap((b: Integer) => toFunctorOps(
      branch(
        leaf(scala.runtime.BoxesRunTime.boxToInteger(Integer2int(b).-(1))), //89,91, 109, 111
        leaf(scala.runtime.BoxesRunTime.boxToInteger(Integer2int(b).+(1)))))(treeM). // 189, 191, 209,211
      map((c: Integer) => c)))
