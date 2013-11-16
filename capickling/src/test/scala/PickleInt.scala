import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import scala.pickling._
import eu.inn.capickling._

class PickleInt extends FlatSpec with Matchers {

  "An integer parameter " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    123456.pickleTo(out)
    890.pickleTo(out)
    verify(m).setInt(0,123456)
    verify(m).setInt(1,890)
  }

  case class TestInt(intValue: Int)
  "An integer inside case class " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    TestInt(123456).pickleTo(out)
    verify(m).setInt("intValue",123456)
  }

  "An integer " should " be read from " in {
    val m = mock[Row]
    when(m.getInt("intValue")).thenReturn(123456)
    val t = m.unpickle[TestInt]
    t.intValue should be(123456)
  }
}