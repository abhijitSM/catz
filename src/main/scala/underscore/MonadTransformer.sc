import scala.concurrent.{Await, Future}

import cats.data.EitherT
import cats.instances.future._
import cats.instances.string._
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.applicative._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

type Response[A] = EitherT[Future, String, A]

def getPowerLevel(autobot: String): Response[Int] = {
  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )
  powerLevels.get(autobot).
    fold(EitherT(Future.successful(s"$autobot unreachable".asLeft[Int])))(_.pure[Response])
}

def canSpecialMove(
  ally1: String,
  ally2: String
): Response[Boolean] = for {
  avg1 <- getPowerLevel(ally1)
  avg2 <- getPowerLevel(ally2)
} yield avg1 + avg2 > 15

def tacticalReport(
  ally1: String,
  ally2: String
): String = {
  val fut = canSpecialMove(ally1, ally2).value
  Await.result(fut, 5.seconds) match {
    case Right(check) => if (check) {
      s"$ally1 and $ally2 are ready to roll out!"
    } else {
      s"$ally1 and $ally2 need a recharge"
    }
    case Left(err) => err
  }
}

tacticalReport("Jazz", "Bumblebee")
tacticalReport("Bumblebee", "Hot Rod")
tacticalReport("Jazz", "Ironhide")
