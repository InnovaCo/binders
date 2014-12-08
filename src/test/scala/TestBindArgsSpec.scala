import eu.inn.binders.naming.{CamelCaseToSnakeCaseConverter, PlainConverter}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestBindArgsSpec extends FlatSpec with Matchers {

  "TestStatement " should " should bind arguments by index" in {
    val stmt = mock[TestSerializer[PlainConverter]]
    val noneInt: Option[Int] = None
    stmt.bindArgs(10, Some(3), noneInt)
    verify(stmt).setInt(0, 10)
    verify(stmt).setIntNullable(1, Some(3))
    verify(stmt).setIntNullable(2, None)
  }

  "TestStatement " should " should bind arguments from case class by name" in {
    val stmt = mock[TestSerializer[PlainConverter]]
    stmt.bind(TestInt(10, Some(555), 20))
    verify(stmt).setInt("intValue1", 10)
    verify(stmt).setIntNullable("nullableValue", Some(555))
    verify(stmt).setInt("intValue2", 20)
  }

  "TestStatement " should " should bind arguments from case class by name with specified naming convention" in {
    val stmt = mock[TestSerializer[CamelCaseToSnakeCaseConverter]]
    stmt.bind(TestInt(10, Some(555), 20))
    verify(stmt).setInt("int_value1", 10)
    verify(stmt).setIntNullable("nullable_value", Some(555))
    verify(stmt).setInt("int_value2", 20)
  }
}