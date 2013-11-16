import com.datastax.driver.core.{Session, Cluster, Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import eu.inn.capickling._
import scala.pickling._

class ConnectionStatementSpec extends FlatSpec with Matchers {

  def fixture = new {
    val cluster = Cluster.builder().addContactPoint("127.0.0.1").build()
    val session = cluster.connect("capickling_tests")

    val yesterday = {
      import java.util._
      val cal = Calendar.getInstance()
      cal.setTime(new Date())
      cal.add(Calendar.DATE, -11)
      cal.getTime()
    }

    val insert = new ConnectionStatement(session, "insert into users(userId, name, created) values (10,'maga', ?)")
    insert.query(yesterday)

    val insert2 = new ConnectionStatement(session, "insert into users(userId, name, created) values (11,'alla', ?)")
    insert2.query(yesterday)
  }

  case class User(userId: Int, name: String, created: java.util.Date)

  "ConnectionStatement " should " be able to execute command " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "delete from users where userid=10")
    stmt.query
  }

  "ConnectionStatement " should " be able to execute with parameters " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "delete from users where userid=?")
    stmt.query(10)
  }

  "ConnectionStatement " should " be able to execute with 2 primitive parameters " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "delete from users where userid in(?,?)")
    stmt.query(10, 11)
  }

  "ConnectionStatement " should " be able to select one row " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name,created from users where userid=10")
    val user = stmt.query.unpickleOne[User]

    assert(user.isDefined)
    assert(user.get.userId == 10)
    assert(user.get.name == "maga")
    assert(user.get.created == f.yesterday)
  }

  "ConnectionStatement " should " be able to select one row with parameters " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name,created from users where userid=?")
    val user = stmt.query(11).unpickleOne[User]

    assert(user.isDefined)
    assert(user.get.userId == 11)
    assert(user.get.name == "alla")
    assert(user.get.created == f.yesterday)
  }

  "ConnectionStatement " should " be able to select two rows with 2 plain parameters " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name,created from users where userid in(?,?)")
    val users = stmt.query(10, 11).unpickleAll[User].toSeq
    assert(users.length == 2)
  }

  "ConnectionStatement " should " be able to select rows " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name,created from users where userid in (10,11)")
    val users = stmt.query.unpickleAll[User]

    assert(users.length == 2)
  }

  "ConnectionStatement " should " be able to select 0 rows " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name,created from users where userid in (12,13)")
    val users = stmt.query.unpickleAll[User]

    assert(users.length == 0)
  }

  /*
  this doesn't work, because :userId can't be set as a list
  case class UserListParams(users: Array[Int])
  "ConnectionStatement " should " be able to select rows with List parameter" in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name from users where userid in (:users)")

    val params = UserListParams(Array(10,11))
    val users = stmt.selectWith[UserListParams,User](params)

    assert(users.length == 2)
  }*/
}