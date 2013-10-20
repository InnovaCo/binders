import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import scala.pickling._
import com.maqdev.capickling._

class PickleInt extends FlatSpec with Matchers {

  case class TestInt(intValue: Int)
  "An integer " should " be bound " in {
    val m = mock[BoundStatement]
    TestInt(123456).pickleTo(m)
    verify(m).setInt("intValue",123456)
  }

  "An integer " should " be read from " in {
    val m = mock[Row]
    when(m.getInt("intValue")).thenReturn(123456)
    val t = m.unpickle[TestInt]
    t.intValue should be(123456)
  }
}