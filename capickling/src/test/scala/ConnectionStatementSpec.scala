import com.datastax.driver.core.{Session, Cluster, Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import com.maqdev.capickling._
import scala.pickling._

class ConnectionStatementSpec extends FlatSpec with Matchers {
  def fixture = new {
    val cluster = Cluster.builder().addContactPoint("127.0.0.1").build()
    val session = cluster.connect("capickling_tests")

    val insert = new ConnectionStatement(session, "insert into users(userId, name) values (10,'maga')")
    insert.execute

    val insert2 = new ConnectionStatement(session, "insert into users(userId, name) values (11,'alla')")
    insert2.execute
  }

  "ConnectionStatement " should " be able to execute command " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "delete from users where userid=10")
    stmt.execute
  }

  case class params(userId: Int)
  "ConnectionStatement " should " be able to execute with parameters " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "delete from users where userid=?")
    stmt.executeWith(params(10))
  }

  case class User(userId: Int, name: String)

  "ConnectionStatement " should " be able to select one row " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name from users where userid=10")
    val user = stmt.selectOne[User]

    assert(user.isDefined)
    assert(user.get.userId == 10)
    assert(user.get.name == "maga")
  }

  "ConnectionStatement " should " be able to select one row with parameters " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name from users where userid=?")
    val user = stmt.selectOneWith[params,User](params(11))

    assert(user.isDefined)
    assert(user.get.userId == 11)
    assert(user.get.name == "alla")
  }

  "ConnectionStatement " should " be able to select rows " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name from users where userid in (10,11)")
    val users = stmt.select[User]

    assert(users.length == 2)
  }

  "ConnectionStatement " should " be able to select 0 rows " in {
    val f = fixture
    val stmt = new ConnectionStatement(f.session, "select userId,name from users where userid in (12,13)")
    val users = stmt.select[User]

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