import scala.pickling._
import eu.inn.capickling._

class Db(session: com.datastax.driver.core.Session) {
  // class for binding input/output parameters
  case class User(userId: Int, name: String)

  lazy val insertStatement = new ConnectionStatement(session,
    "insert into users(userid, name) values (?, ?)")

  def insertUser(user: User) = insertStatement.query(user)

  lazy val selectAllStatement = new ConnectionStatement(session,
    "select * from users")

  def selectAllUsers = selectAllStatement.query.unpickleAll[User]

  lazy val selectUserStatement = new ConnectionStatement(session,
    "select * from users where userId = ?")

  def selectUser(userId: Int) = selectUserStatement.query(userId).unpickleOne[User]
}