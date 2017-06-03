object Main {
  case class Reader[Db, T](read: Db => T) {
    def map[U](convert: T => U) : Reader[Db, U] = {
      Reader(read andThen(convert))
    }
    def flatMap[U](toReader: T => Reader[Db, U]) : Reader[Db, U] = {
      Reader[Db, U](db => toReader(read(db)).read(db))
    }
  }

  case class Db(
     userNames: Map[Int, String],
     passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  val database = Db(Map(1 -> "u1", 2 -> "u2"), Map("u1" -> "u1@Password", "u2" -> "u2@Password"))

  def checkUserName(userId: Int) : DbReader[Option[String]] = {
    Reader(db => db.userNames.get(userId))
  }

  def checkPassword(userName: String, password: String) : DbReader[Boolean] = {
    Reader[Db, Boolean]((db: Db) => db.passwords.get(userName).contains(password))
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    checkUserName(userId).flatMap(userName => checkPassword(userName.getOrElse(""), password))
    /*for {
      userName <- checkUserName(userId)
      isValidLogin <- checkPassword(userName.getOrElse(""), password)
    } yield isValidLogin*/
  }

  def main(args: Array[String]): Unit = {
    val isLoggedIn = checkLogin(1, "u1@Password").read(database)
    println(isLoggedIn)
  }
}