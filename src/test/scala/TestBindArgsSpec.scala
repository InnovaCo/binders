import eu.inn.binders._
import eu.inn.binders.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class TestBindArgsSpec extends FlatSpec with Matchers {
  "TestBindArgsSpec " should " should bind arguments by index" in {
    val stmt = mock[TestSerializer[PlainConverter]]
    val noneInt: Option[Int] = None
    stmt.bindArgs(10, Some(3), noneInt)
    verify(stmt).addInt(10)
    verify(stmt).addIntNullable(Some(3))
    verify(stmt).addIntNullable(None)
  }
}