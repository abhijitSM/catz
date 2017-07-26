import cats.data.Reader
import cats.syntax.applicative._

case class Db(
               usernames: Map[Int, String],
               passwords: Map[String, String]
             )

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))

def checkPassword(
                   username: String,
                   password: String
                 ): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))

def checkLogin(
                userId: Int,
                password: String
              ): DbReader[Boolean] = {
  for {
    user <- findUsername(userId)
    b <- user.map(username => {
      checkPassword(username, password)
    }).getOrElse(false.pure[DbReader])
  } yield b
}

val db = Db(
  Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  ),
  Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )
)

checkLogin(1, "zerocool").run(db)