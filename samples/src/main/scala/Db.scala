	import scala.pickling._
	import eu.inn.capickling._

	class Db(session: com.datastax.driver.core.Session) {
	  // class for binding input/output parameters
	  case class User(userId: Int, name: String)

	  lazy val insertStatement = new ConnectionStatement(session,
	    "insert into users(userid, name) values (?, ?)")

	  def insertUser(user: User) = insertStatement.executeWith(user)

	  lazy val selectAllStatement = new ConnectionStatement(session,
	    "select * from users")

	  def selectAllUsers = selectAllStatement.select[User]
	}