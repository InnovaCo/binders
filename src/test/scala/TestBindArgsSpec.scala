import eu.inn.binders.naming.{CamelCaseToSnakeCaseConverter, PlainConverter}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestBindArgsSpec extends FlatSpec with Matchers {

  "TestStatement " should " should bind arguments by index" in {
    val m = mock[TestStatement[PlainConverter]]
    val q = new TestQuery(m)
    val noneInt: Option[Int] = None
    val stmt = q.createStatement
    stmt.bindArgs(10, Some(3), noneInt)
    verify(m).setInt(0, 10)
    verify(m).setIntNullable(1, Some(3))
    verify(m).setIntNullable(2, None)
  }

  "TestStatement " should " should bind arguments from case class by name" in {
    val m = mock[TestStatement[PlainConverter]]
    val q = new TestQuery(m)
    val stmt = q.createStatement
    stmt.bindClass(TestInt(10, Some(555), 20))
    verify(m).setInt("intValue1", 10)
    verify(m).setIntNullable("nullableValue", Some(555))
    verify(m).setInt("intValue2", 20)
  }

  "TestStatement " should " should bind arguments from case class by name with specified naming convention" in {
    val m = mock[TestStatement[CamelCaseToSnakeCaseConverter]]
    val q = new TestQuery(m)
    val stmt = q.createStatement
    stmt.bindClass(TestInt(10, Some(555), 20))
    verify(m).setInt("int_value1", 10)
    verify(m).setIntNullable("nullable_value", Some(555))
    verify(m).setInt("int_value2", 20)
  }
}